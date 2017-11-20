{-# LANGUAGE OverloadedStrings #-}
module Paradox.Functions.Math.TimeSeriesChunk (
  divTimeSeriesChunk
, addTimeSeriesChunk
, subTimeSeriesChunk
, divTimeSeriesChunkScalar
, mulTimeSeriesChunkScalar
, offsetTimeSeriesChunk
, cmpGTTimeSeriesChunk
, getSampleDeviationTS
, sortTSByAvg
, getPercentile
) where

import Prelude                           hiding (sum)

import Paradox.Istatd.Types

import Data.List.NonEmpty                (NonEmpty)
import Data.Function                     (on)
import Data.Foldable                     (sum)

import qualified Data.List.NonEmpty      as NonEmpty

divTimeSeriesChunkScalar :: Double
                         -> TimeSeriesChunk
                         -> TimeSeriesChunk
divTimeSeriesChunkScalar divisor tsc =
    tsc { tscAvg = tscAvg tsc / divisor
        , tscSdev = tscSdev tsc / divisor
        , tscSum = tscSum tsc / divisor
        , tscSumsq = tscSumsq tsc / divisor
        , tscMin = tscMin tsc / divisor
        , tscMax = tscMax tsc / divisor
        }


mulTimeSeriesChunkScalar :: Double
                         -> TimeSeriesChunk
                         -> TimeSeriesChunk
mulTimeSeriesChunkScalar factor tsc =
    tsc { tscAvg = tscAvg tsc * factor
        , tscSdev = tscSdev tsc * factor
        , tscSum = tscSum tsc * factor
        , tscSumsq = tscSumsq tsc * factor
        , tscMin = tscMin tsc * factor
        , tscMax = tscMax tsc * factor
        }

divTimeSeriesChunk :: TimeSeriesChunk
                   -> TimeSeriesChunk
                   -> TimeSeriesChunk
divTimeSeriesChunk lTSC rTSC =
    lTSC { tscAvg = tscAvg lTSC / tscAvg rTSC
         , tscSdev = 0
         , tscSum = tscSum lTSC / tscSum rTSC
         , tscSumsq = tscSumsq lTSC / tscSumsq rTSC
         , tscMin = tscMin lTSC / tscMin rTSC
         , tscMax = tscMax lTSC / tscMax rTSC
         }

addTimeSeriesChunk :: TimeSeriesChunk
                   -> TimeSeriesChunk
                   -> TimeSeriesChunk
addTimeSeriesChunk lTSC rTSC =
    lTSC { tscAvg = newAvg
         , tscMax = tscMax lTSC + tscMax rTSC
         , tscMin = tscMin lTSC + tscMin rTSC
         , tscCount = totalCount
         , tscSumsq = sumsq
         , tscSum = tscSum lTSC + tscSum rTSC
         , tscSdev = sqrt $ abs $ tscSdev lTSC**2 + tscSdev rTSC**2
         }
    where
        sumsq = tscSumsq lTSC + tscSumsq rTSC
        totalCount = tscCount lTSC + tscCount rTSC
        newAvg = tscAvg lTSC + tscAvg rTSC

offsetTimeSeriesChunk :: Double
                      -> TimeSeriesChunk
                      -> TimeSeriesChunk
offsetTimeSeriesChunk offset tsc =
    tsc { tscAvg = tscAvg tsc + offset
        , tscMax = tscMax tsc + offset
        , tscMin = tscMin tsc + offset
        , tscSum = tscSum tsc + offset
        }

subTimeSeriesChunk :: TimeSeriesChunk
                   -> TimeSeriesChunk
                   -> TimeSeriesChunk
subTimeSeriesChunk lTSC rTSC =
    lTSC { tscAvg = newAvg
         , tscMax = tscMax lTSC - tscMax rTSC
         , tscMin = tscMin lTSC - tscMin rTSC
         , tscCount = totalCount
         , tscSumsq = sumsq
         , tscSum = tscSum lTSC - tscSum rTSC
         , tscSdev = sqrt $ abs $ tscSdev lTSC**2 + tscSdev rTSC**2
         }
    where
        sumsq = tscSumsq lTSC - tscSumsq rTSC
        totalCount = tscCount lTSC - tscCount rTSC
        newAvg = tscAvg lTSC - tscAvg rTSC

cmpGTTimeSeriesChunk :: (TimeSeriesChunk -> Double)
                     -> TimeSeriesChunk
                     -> TimeSeriesChunk
                     -> TimeSeriesChunk
cmpGTTimeSeriesChunk f lTSC rTSC =
    if f lTSC > f rTSC then
         lTSC
    else
         rTSC

sortTSByAvg  :: NonEmpty TimeSeriesChunk
             -> NonEmpty TimeSeriesChunk
sortTSByAvg = NonEmpty.sortBy (compare `on` tscAvg)

getPercentileTSC :: Int
                 -> NonEmpty TimeSeriesChunk
                 -> TimeSeriesChunk
getPercentileTSC p ts =
    let sortedTS = sortTSByAvg ts
        rank :: Double
        rank = (fromIntegral p / 100.0) * fromIntegral (NonEmpty.length sortedTS)
        rankI = floor $ maximum [0, rank - 1]
    in sortedTS NonEmpty.!! rankI

getPercentile :: Int
              -> NonEmpty TimeSeriesChunk
              -> Double
getPercentile p ts =  tscAvg $ getPercentileTSC p ts

getSampleDeviationTS :: NonEmpty TimeSeriesChunk
                     -> (Double, Double)
getSampleDeviationTS ts =
    let avgs = NonEmpty.map tscAvg ts
        l = fromIntegral $ NonEmpty.length avgs
        s = sum avgs
        m = s / l
        diffs = NonEmpty.map (\x -> (x - m)**2 ) avgs
        msq = sum diffs / fromIntegral (NonEmpty.length diffs - 1)
        sdev = sqrt msq
    in (sdev, m)
