{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Paradox.Memo.Internal where

import Prelude                                  hiding (lookup)

import Data.Maybe                               (isNothing)

import Control.Monad                            ( when
                                                , void
                                                , forever
                                                )
import Control.Concurrent.STM
import Control.Concurrent.Async

import Control.Concurrent                       (threadDelay)
import Data.Semigroup                           ((<>))

import qualified STMContainers.Map              as STM
import qualified Data.Time.Clock.POSIX          as POSIX

import qualified Paradox.Istatd.AsyncClient     as Istatd
import qualified Imvu.Network.IstatdClient      as Istatd (Name)
import Paradox.Logger.Global          (Log (..) )
import qualified Paradox.Logger.Global          as G

data Expirable a = Expirable {
          item :: a
        , ts :: POSIX.POSIXTime
        , killer :: Async ()
    }

class (STM.Key a) => FuzzyKey a where
    fkey :: a -> a
    expiry :: a -> Int

-- | maybe cache needs to be an LRU? could see lots of 1h bucket requests cause massive memory explosion
data ExpirableCache k v = ECache {
      ecCount :: !(TVar Int)
    , ecMap :: STM.Map k (Expirable v)
    }



type MemoFn m k v = (k -> m v) -> k -> IO v

data Issue = Hit | Miss | ThreadSpawned | ThreadCancelled | ThreadFinished | Insert | Delete | Size Int deriving Show

find :: STM.Key k => k -> ExpirableCache k v -> IO (Maybe (Expirable v))
find k ECache {ecMap} = atomically $ STM.lookup k ecMap

insert :: FuzzyKey k => (Issue -> IO ()) -> k -> v -> ExpirableCache k v -> IO ()
insert logger k v state@ECache {..} = do
    t <- POSIX.getPOSIXTime
    killer <- setupKiller logger k state
    let e = Expirable { ts = t, item = v , killer = killer }
    logger Insert
    atomically $ do
        STM.insert e k ecMap
        modifyTVar' ecCount (+1)

update :: FuzzyKey k => (Issue -> IO ()) -> k -> Expirable v -> ExpirableCache k v -> IO ()
update logger k oldval state@ECache {..} = do
    t <- POSIX.getPOSIXTime
    cancel $ killer oldval
    logger ThreadCancelled
    killer <- setupKiller logger k state
    let e = Expirable { ts = t, item = item oldval, killer = killer }
    logger Insert
    atomically $ do
        ex <- STM.lookup k ecMap
        STM.insert e k ecMap
        when (isNothing ex) $
            modifyTVar' ecCount (+1)

fuzzyMemo_ :: (FuzzyKey k) => (Issue -> IO ()) -> (forall a. m a -> IO a) -> ExpirableCache k v -> MemoFn m k v
fuzzyMemo_ logger run state =
    let memoFn f k = do
            found' <- find (fkey k) state
            case found' of
                Nothing -> do
                    logger Miss
                    r <- run $ f k
                    insert logger (fkey k) r state
                    return r
                Just e@Expirable {item} -> do
                    logger Hit
                    update logger (fkey k) e state
                    return item
    in memoFn

fuzzyMemo :: (FuzzyKey k) => Istatd.Send -> Istatd.Name -> (forall a. m a -> IO a) -> IO (MemoFn m k v)
fuzzyMemo logger pfx = fuzzyMemo' (logIssue logger pfx)

fuzzyMemo' :: (FuzzyKey k) => (Issue -> IO ()) -> (forall a. m a -> IO a) -> IO (MemoFn m k v)
fuzzyMemo' logger run = do
   ecMap <- atomically STM.new
   ecCount <- atomically $ newTVar 0
   let query = fuzzyMemo_ logger run ECache {..}
   void $ async $ forever $ do
        l <- atomically $ readTVar ecCount
        logger $ Size l
        threadDelay 5000000
   return query

setupKiller :: FuzzyKey k => (Issue -> IO ()) -> k -> ExpirableCache k v -> IO (Async ())
setupKiller logger k ECache {..} = async $ do
    logger ThreadSpawned
    threadDelay $ expiry k
    logger Delete
    G.logTraceG $ LogText "We are expiring a counter"
    atomically $ do
        STM.delete k ecMap
        modifyTVar' ecCount (\x -> x - 1)
    logger ThreadFinished

logIssue :: Istatd.Send -> Istatd.Name -> Issue -> IO ()
logIssue logger pfx = \case
    Hit ->
        logger $ Istatd.Counter $ pfx <> "hit"
    Miss ->
        logger $ Istatd.Counter $ pfx <> "miss"
    ThreadSpawned ->
        logger $ Istatd.Counter $ pfx <> "thread" <> "spawned"
    ThreadCancelled ->
        logger $ Istatd.Counter $ pfx <> "thread" <> "cancelled"
    ThreadFinished ->
        logger $ Istatd.Counter $ pfx <> "thread" <> "finished"
    Insert ->
        logger $ Istatd.Counter $ pfx <> "insert"
    Delete ->
        logger $ Istatd.Counter $ pfx <> "delete"
    Size n ->
        logger $ Istatd.Gauge (pfx <> "size") (fromIntegral n)
