module Paradox.Functions.Math.TimeSeries (
  timeSeriesZip
, divTS
, addTS
, subTS
, cmpGTTS
, divTSScalar
) where

import Paradox.Istatd.Types

import Data.List.NonEmpty                           (NonEmpty(..))
import Paradox.Functions.Math.TimeSeriesChunk       ( divTimeSeriesChunkScalar
                                                    , divTimeSeriesChunk
                                                    , addTimeSeriesChunk
                                                    , subTimeSeriesChunk
                                                    , cmpGTTimeSeriesChunk
                                                    )

import qualified Data.List.NonEmpty                 as NonEmpty

-- |Zip together two time series lists using func accounting for gaps.
-- The func will be run if the elements times are the same, otherwise
-- the earlier one will be kept. Both lists must terminate
timeSeriesZip :: (TimeSeriesChunk -> TimeSeriesChunk -> TimeSeriesChunk)
              -> NonEmpty TimeSeriesChunk
              -> NonEmpty TimeSeriesChunk
              -> NonEmpty TimeSeriesChunk
timeSeriesZip iF il ir =
    let il' = NonEmpty.toList il
        ir' = NonEmpty.toList ir
    in NonEmpty.fromList $ go iF il' ir'
    where
        go _ al@(_:_) [] = foldr (:) [] al
        go f al@(l:ls) ar@(r:rs) = case tscTime l `compare` tscTime r of
            EQ -> f l r : go f ls rs
            LT -> l : go f ls ar
            GT -> r : go f al rs
        go _ [] ar@(_:_) = foldr (:) [] ar
        go _ _ _ = []

mathOpOnTwoTimeSeries :: (TimeSeriesChunk -> TimeSeriesChunk -> TimeSeriesChunk)
                      -> CounterName
                      -> ParadoxTimeSeries
                      -> ParadoxTimeSeries
                      -> ParadoxTimeSeries
mathOpOnTwoTimeSeries f newName lTS rTS =
    let leftData = ptsData lTS
        leftStartTime = tscTime $ NonEmpty.head leftData

        rightData = ptsData rTS
        rightStartTime = tscTime $ NonEmpty.head rightData

        (realLeftTS, realRightTS, newOp) = if leftStartTime <= rightStartTime
            then (lTS, rTS, f)
            else (rTS, lTS, flip f)

        realLeftData = ptsData realLeftTS
        realRightData = ptsData realRightTS
    in go newOp realLeftTS newName realLeftData realRightData
--    | null (ptsData lTS) = rTS { ptsName = newName }
--    | null (ptsData rTS) = lTS { ptsName = newName }
    where
        go newOp base n l r = base { ptsData = timeSeriesZip newOp l r
                                   , ptsName = n
                                   }

divTS :: CounterName
      -> ParadoxTimeSeries
      -> ParadoxTimeSeries
      -> ParadoxTimeSeries
divTS = mathOpOnTwoTimeSeries divTimeSeriesChunk

addTS :: CounterName
      -> ParadoxTimeSeries
      -> ParadoxTimeSeries
      -> ParadoxTimeSeries
addTS = mathOpOnTwoTimeSeries addTimeSeriesChunk

subTS :: CounterName
      -> ParadoxTimeSeries
      -> ParadoxTimeSeries
      -> ParadoxTimeSeries
subTS = mathOpOnTwoTimeSeries subTimeSeriesChunk

cmpGTTS :: (TimeSeriesChunk -> Double)
        -> CounterName
        -> ParadoxTimeSeries
        -> ParadoxTimeSeries
        -> ParadoxTimeSeries
cmpGTTS op = mathOpOnTwoTimeSeries $ cmpGTTimeSeriesChunk op

divTSScalar :: CounterName
            -> Double
            -> ParadoxTimeSeries
            -> ParadoxTimeSeries
divTSScalar newName divisor ts =
    ts { ptsData = NonEmpty.map (divTimeSeriesChunkScalar divisor)
                                (ptsData ts)
       , ptsName = newName
       }
