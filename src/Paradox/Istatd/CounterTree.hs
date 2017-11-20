{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Paradox.Istatd.CounterTree (
  SuperHashMap (..)
, IValue (..)
, CounterType (..)
, getCounterType
, lookupMany
, lookupManyT
, construct
, empty
) where

import Control.Monad
import Data.Aeson

import Data.HashMap.Strict              (HashMap)

import qualified Data.Text              as TS
import qualified Data.HashMap.Strict    as H

data CounterType  = Gauge | Counter | Aggregate
                  deriving (Show, Eq)

data IValue       = IValue { isLeaf :: Bool
                           , _counterType :: CounterType
                           } deriving (Show, Eq)

type InnerVal     = (Maybe IValue, SuperHashMap)
type InnerHash    = HashMap TS.Text InnerVal
data SuperHashMap = SuperHashMap InnerHash
                  deriving (Show, Eq)

instance ToJSON SuperHashMap where
    toJSON = go
        where
        go (SuperHashMap h) = toJSON h

instance {-# OVERLAPPING #-} ToJSON InnerVal where
    toJSON = go
        where
        go (Nothing, c) = object [ "c" .= toJSON c ]
        go (Just (IValue _ t), c@(SuperHashMap h)) =
            if H.null h
                then object [
                      "t" .= counterType t
                    ]
                else object [
                      "c" .= toJSON c
                    , "t" .= counterType t
                    ]


instance ToJSON IValue where
    toJSON = go
        where
        go (IValue l c) = object [
              "l" .= l
            , "t" .= counterType c
            ]

empty :: SuperHashMap
empty = SuperHashMap mempty

leaf :: TS.Text
     -> IValue
     -> SuperHashMap
leaf t v = SuperHashMap $ H.singleton t (Just v, empty)

union :: SuperHashMap
      -> SuperHashMap
      -> SuperHashMap
union !x !y = case (x, y) of
  (SuperHashMap !hx, SuperHashMap !hy) ->
      SuperHashMap $ H.unionWith (\(!i, !l) (!i', !r) -> (i `mplus` i', l `union` r)) hx hy

construct :: [([TS.Text], IValue)]
          -> SuperHashMap
construct = foldr1 union . map (uncurry fromPath)

fromPath :: [TS.Text]
         -> IValue
         -> SuperHashMap
fromPath !keys !v
  = foldr (\(!k) (!v') -> SuperHashMap $ H.singleton k (Nothing, v') ) (leaf (last keys) v) (init keys)

-- get all the values along the path and also return the subtree
lookupMany :: [TS.Text]
           -> SuperHashMap
           -> Maybe ([(TS.Text, Maybe IValue)], HashMap TS.Text (Maybe IValue, SuperHashMap))
lookupMany keys (SuperHashMap h) = go [] keys h where
  go acc []     hashMap = return (acc, hashMap)
  go acc ["*"]  hashMap = go acc [] hashMap
  go acc (x:xs) hashMap = do
    (mv, SuperHashMap h') <- H.lookup x hashMap
    go ((x, mv) : acc) xs h'

lookupManyT :: [TS.Text]
           -> SuperHashMap
           -> Maybe ([(TS.Text, Maybe IValue)], SuperHashMap)
lookupManyT keys shm@SuperHashMap {} = go [] keys shm where
  go acc []     c = return (acc, c)
  go acc ["*"]  c = go acc [] c
  go acc (x:xs) (SuperHashMap hashMap) = do
    (mv, c'@SuperHashMap {}) <- H.lookup x hashMap
    go ((x, mv) : acc) xs c'


counterType :: CounterType -> Int
counterType Gauge = 0
counterType Counter = 1
counterType Aggregate = 2

getCounterType :: Int -> Maybe CounterType
getCounterType 0 = Just Gauge
getCounterType 1 = Just Counter
getCounterType 2 = Just Aggregate
getCounterType _ = Nothing
