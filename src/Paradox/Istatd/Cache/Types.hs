{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Paradox.Istatd.Cache.Types where

import GHC.Generics

import Data.Hashable
import Paradox.Istatd.Types
import Paradox.Memo (FuzzyKey(..))
import qualified Paradox.Istatd.Lifted as IstatdLifted
import qualified Data.Time.Clock.POSIX as POSIX

data CountersReq = CountersReq {
          csrMaxSamples :: Int
        , csrTimeRange :: TimeRange
        , csrKeys :: [CounterSpec]
    } deriving (Show, Eq, Generic)

instance FuzzyKey CountersReq where
    fkey cr@CountersReq { csrMaxSamples , csrTimeRange } =
        let s = floor $ POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper $ trStart csrTimeRange
            e = floor $ POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper $ trStop csrTimeRange
            i' = (e - s) `div` csrMaxSamples
            i = floor $ IstatdLifted.reduction $ fromIntegral i'
        in cr { csrTimeRange = IstatdLifted.normalizeRange csrTimeRange i }
    -- | Expiry should likely be capped.... esp for counters with very high reduction
    expiry CountersReq { csrMaxSamples, csrTimeRange } =
        let s = floor $ POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper $ trStart csrTimeRange
            e = floor $ POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper $ trStop csrTimeRange
            i' = (e - s) `div` csrMaxSamples
            i = floor $ IstatdLifted.reduction $ fromIntegral i'
        in min 3600 i * 1000000

instance Hashable CountersReq

