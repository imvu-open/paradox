{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Paradox.Cache.Internal where

import Prelude                                  hiding (lookup)

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.IO.Class


import Control.Concurrent                       (threadDelay)
import Data.Maybe                               (isJust)
import Control.Monad                            ( forM
                                                , forM_
                                                , void
                                                , forever
                                                )
import Data.List                                (partition)

import Paradox.Memo.Internal                    ( insert
                                                , update
                                                , find
                                                , ExpirableCache (..)
                                                , FuzzyKey (..)
                                                , Expirable (..)
                                                , Issue (..)
                                                , logIssue
                                                )

import qualified Data.List as L
import qualified Data.Text                      as TS

import qualified STMContainers.Map              as STM

import qualified Paradox.Istatd.AsyncClient     as Istatd
import qualified Imvu.Network.IstatdClient      as Istatd (Name)
import Paradox.Logger.Global          (Log (..) )
import qualified Paradox.Logger.Global          as G


data CacheFn k v = Cache { get :: k -> IO (Maybe v)
                         , set :: k -> v -> IO ()
                         }

-- | This function does some crazy caching. It will first lookup the key to see if there is an exact
-- saved result. If there is it will recache it, and cache intermediate values as well, using valueToValues to
-- lens in and find what else should be cached.
-- Additionally it will lookup subkeys of the key in cache to and use keyToResultF to do a batched request for
-- missing keys.
-- At the end it will cache 1) The full data (Cached and Requested) under key, 2) Each individual data
-- (Cached, Requested), where requested is broken up with valueToValues.
-- The cache will be checked for 1) a perfect match of cached data for key, or 2) by lookup up they
-- subkeys generated with subKeysFn from key, and then using missingKeysToKey to generate a new key and
-- using keyToResultF to request the data for that key.
-- k and k2, and v and v2 must be the same type. They are jsut differnetiated for clarity.
cacheEx :: (MonadIO m, k ~ k2, v ~ v2)
        => CacheFn k v              -- ^ The state of our cache and functions to access / set
        -> k                        -- ^ The master key. Will be looked up first, and final result will be cached
        -> (k -> [k2])              -- ^ Function from key to subkeys of the data to look up seperately in cache
        -> ([k2] -> Maybe k)        -- ^ Function to combine keys to request keys missing from cache
        -> (k -> m v)               -- ^ Function to request data using missing keys combined
        -> (k -> v -> [(k2,v2)])    -- ^ Function to partition value into sub values for caching
        -> ([v2] -> v)              -- ^ Function to combine values into a single value for return
        -> m v
cacheEx Cache {..} key subKeysFn missingKeysToKey keyToResultF valueToValues combineValues = do
    let cacheIt d = forM_ d $ \(k, v) -> liftIO $ set k v

    allData <- liftIO $ get key
    case allData of
      Just ddata -> do
            G.logVerboseGM $ LogText "Found all data"
            let toCache' = valueToValues key ddata
                toCache = (key, ddata):toCache'
            cacheIt toCache
            return ddata
      Nothing -> do
        let subKeys = subKeysFn key
        cachedKeys <- forM subKeys $ \k -> do
            mv <- liftIO $ get k
            return (k, mv)
        let (found,missing) = partition (\(_,v) -> isJust v) cachedKeys
            foundForCache = [(k,v) | (k, Just v) <- found]
            missing' = map fst missing

            missingKey = missingKeysToKey missing'

        case missingKey of
            Just missingKey' -> do
                res <- keyToResultF missingKey'
                G.logVerboseGM $ LogText "Needed to request some data"

                let toCache' = valueToValues key res
                    allValues = combineValues $ map snd $ foundForCache ++ toCache'
                    toCache = L.concat [foundForCache, toCache', [(key, allValues)]]
                cacheIt toCache
                return allValues
            Nothing -> do
                G.logVerboseGM $ LogText "All data from cache"
                let allValues = combineValues $ map snd foundForCache
                    toCache = (key, allValues):foundForCache
                cacheIt toCache
                return allValues




--caches the result with a simple function
cacheRes :: (MonadIO m) => CacheFn k v -> k -> (k -> m v) -> m v
cacheRes Cache {..} key fn = do
    ex <- liftIO $ get key
    case ex of
      Just ex' -> return ex'
      Nothing -> do
          val <- fn key
          liftIO $ set key val
          return val


fuzzyCache_ :: FuzzyKey k => (Issue -> IO ()) -> ExpirableCache k v -> CacheFn k v
fuzzyCache_ logger state =
    let getCacheFn k = do
            found' <- find (fkey k) state
            case found' of
                Nothing -> do
                    logger Miss
                    return Nothing
                Just e@Expirable {item} -> do
                    logger Hit
                    update logger (fkey k) e state
                    return $ Just item
        setCacheFn k v = do
            found' <- find (fkey k) state
            case found' of
                Nothing -> insert logger (fkey k) v state
                Just e@Expirable {} -> update logger (fkey k) (e { item = v }) state

    in Cache { get = getCacheFn, set = setCacheFn }

fuzzyCache :: FuzzyKey k => Istatd.Send -> Istatd.Name -> IO (CacheFn k v)
fuzzyCache logger pfx = fuzzyCache' (logIssue logger pfx)

fuzzyCache' :: FuzzyKey k => (Issue -> IO ()) -> IO (CacheFn k v)
fuzzyCache' logger = do
   ecMap <- atomically STM.new
   ecCount <- atomically $ newTVar 0
   let query = fuzzyCache_ logger ECache {..}
   void $ async $ forever $ do
        l <- atomically $ readTVar ecCount
        logger $ Size l
        G.logTraceG $ LogText (TS.concat ["Size of cache is ",  TS.pack $ show l])
        threadDelay 5000000
   return query
