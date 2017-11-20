module Paradox.Istatd.Lifted where

import Paradox.Istatd.Types
import Data.Time.Clock.POSIX

-- | Istatd attempts to normalize bucket start and stop times to align with intervals
normalizeRange :: TimeRange -> Int -> TimeRange
normalizeRange t i =
    let start = floor $ utcTimeToPOSIXSeconds $ unTimeWrapper $ trStart t
        stop = floor $ utcTimeToPOSIXSeconds $ unTimeWrapper $ trStop t
        newStart = start - (start `mod` i)
        stopOverrun = stop `mod` i
        newStop = stop - stopOverrun + if stopOverrun /= 0
            then i
            else 0
    in t {
          trStart = TimeWrapper . posixSecondsToUTCTime . fromIntegral $ newStart
        , trStop = TimeWrapper . posixSecondsToUTCTime . fromIntegral $ newStop
        }

-- | Calculate interval reduction in the same way istatd does
reduction :: Double -> Double
reduction ss =
    let maxReduction = 86400
        reductions = [10,20,30,60,120,300,600,900,1200,1800,3600,2*3600,3*3600,4*3600,6*3600,8*3600,12*3600,maxReduction]
    in minimum $ (maxReduction:) $ dropWhile (<= ss) reductions

