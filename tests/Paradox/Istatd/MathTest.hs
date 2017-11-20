{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Paradox.Istatd.MathTest where


import Prelude hiding (foldl)
import Control.Applicative ((<$>))
import Data.Foldable (foldl)
import Paradox.Functions
import Paradox.Functions.Math.TimeSeriesChunk      (addTimeSeriesChunk)
import Paradox.Functions.Math.TimeSeries      (timeSeriesZip)
import Paradox.Functions.Util      (Summarizer(..))
import Paradox.Istatd.Types
import Paradox.Istatd.Functions (magic, rekey, correctTimeSeriesIntervals, correctIntervals)
import Language.Paradox.Eval.Types

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Default

import Test.Tasty
import Test.Tasty.TH

import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)

import Data.Monoid (mempty)

import qualified Data.Map.Strict as M
import qualified Data.Text as TS
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty ((<|))

secondsToUTC :: (Integral a) => a -> UTCTime
secondsToUTC = posixSecondsToUTCTime . fromIntegral

mkExpectedTimeSeries :: TS.Text -> Int -> Int -> Int -> [Double] -> ParadoxTimeSeries
mkExpectedTimeSeries name s e i vals = ParadoxTimeSeries {
          ptsRange = TimeRangeE { treStart = TimeWrapper $ secondsToUTC (s * i), treEnd = TimeWrapper $ secondsToUTC (e * i) }
        , ptsInterval = i
        , ptsName = name
        , ptsData = NonEmpty.fromList $ reverse $ foldl (\a (x,v) -> mkExpectedTimeSeriesChunk (x * i) v :a) [] $ zip [s..e] vals
        , ptsDrawOptions = mempty
    }

mkExpectedTimeSeriesChunk :: Int -> Double -> TimeSeriesChunk
mkExpectedTimeSeriesChunk x val = let
            in TimeSeriesChunk {
                      tscTime = fromIntegral x
                    , tscCount = 1
                    , tscMin = val
                    , tscMax = val
                    , tscAvg = val
                    , tscSum = val
                    , tscSumsq = val
                    , tscSdev = val
                }

mkTimeRange :: POSIXTime -> POSIXTime -> TimeRange
mkTimeRange l r = TimeRange { trStart = TimeWrapper $ posixSecondsToUTCTime l, trStop = TimeWrapper $ posixSecondsToUTCTime r }

case_movingAverage :: Assertion
case_movingAverage = do
    let ts = mkExpectedTimeSeries tsName 0 10 10 [0,1,2,3,4,5,6,7,8,9,10]
        tsName = "a"
        window = 3
        co = CounterSpec "a" Nothing
        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }
        exName = TS.concat ["movingAverage(", TS.pack $ show window, ",", tsName, ")"]

        Just ts' = M.lookup (co { csName = exName } ) $ timeSeriesMap $ movingAverage wc window $ def { timeSeriesMap = M.singleton co ts }

        ex = mkExpectedTimeSeries exName 2 10 10 [1,2,3,4,5,6,7,8,9]
        ex' = ex { ptsData = foldl (flip (<|))  (ptsData ex) [def,def]}

        avg = NonEmpty.map tscAvg . ptsData
        time = NonEmpty.map tscTime . ptsData

    assertEqual "Length should be 11" 11 (NonEmpty.length $ ptsData ts')
    assertEqual "Expected average computed" (avg ex') (avg ts')
    assertEqual "Expected times computed" (time ex') (time ts')
    assertEqual "Expected name is correct" exName (ptsName ts')

case_medianFilter :: Assertion
case_medianFilter = do
    let ts = mkExpectedTimeSeries tsName 0 10 10 [0,1,1,3,3,5,6,7,7,8,10]
        tsName = "a"
        window = 3
        co = CounterSpec "a" Nothing
        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }
        exName = TS.concat ["medianFilter(", TS.pack $ show window, ",", tsName, ")"]

        Just ts' = M.lookup (co { csName = exName } ) $ timeSeriesMap $ medianFilter wc window $ def { timeSeriesMap = M.singleton co ts }

        ex = mkExpectedTimeSeries exName 1 9 10 [1,1,3,3,5,6,7,7,8]
        ex' = ex { ptsData = NonEmpty.reverse $ def<| NonEmpty.reverse (def<|ptsData ex) }

        avg = NonEmpty.map tscAvg . ptsData
        time = NonEmpty.map tscTime . ptsData

    assertEqual "Length should be 11" 11 (NonEmpty.length $ ptsData ts')
    assertEqual "Expected medians computed" (avg ex') (avg ts')
    assertEqual "Expected times computed" (time ex') (time ts')
    assertEqual "Expected name is correct" exName (ptsName ts')

case_integrate :: Assertion
case_integrate = do
    let ts = mkExpectedTimeSeries tsName 0 10 10 [0,1,2,3,4,5,6,7,8,9,10]
        tsName = "a"
        co = CounterSpec tsName Nothing
        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }
        exName = TS.concat ["integrate(", tsName, ")"]

        Just ts' = M.lookup (co { csName = exName } ) $ timeSeriesMap $ integrify wc $ def { timeSeriesMap = M.singleton co ts }

        ex = mkExpectedTimeSeries exName 0 10 10 [0,1,3,6,10,15,21,28,36,45,55]

        avg = NonEmpty.map tscAvg . ptsData
        time = NonEmpty.map tscTime . ptsData

    assertEqual "Length should be 11" 11 (NonEmpty.length $ ptsData ts')
    assertEqual "Expected integration computed" (avg ex) (avg ts')
    assertEqual "Expected times computed" (time ex) (time ts')
    assertEqual "Expected name is correct" exName (ptsName ts')

case_sum :: Assertion
case_sum = do
    let ts = mkExpectedTimeSeries tsName 0 10 10 [0,1,2,3,4,5,6,7,8,9,10]
        ts2 = mkExpectedTimeSeries tsName2 0 10 10 [1,2,3,4,5,6,7,8,9,10,11]
        tsName = "a"
        tsName2 = "b"
        co = CounterSpec tsName Nothing
        co2 = CounterSpec tsName2 Nothing
        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }
        exName = TS.concat ["sum(", tsName, ",", tsName2, ")"]

        Just ts' = M.lookup (co { csName = exName } ) $ timeSeriesMap $ sumify wc $ def { timeSeriesMap = M.fromList [(co,ts), (co2,ts2)] }

        ex = mkExpectedTimeSeries exName 0 10 10 [1,3,5,7,9,11,13,15,17,19,21]

        avg = NonEmpty.map tscAvg . ptsData
        time = NonEmpty.map tscTime . ptsData

    assertEqual "Length should be 11" 11 (NonEmpty.length $ ptsData ts')
    assertEqual "Expected sum computed" (avg ex) (avg ts')
    assertEqual "Expected times computed" (time ex) (time ts')
    assertEqual "Expected name is correct" exName (ptsName ts')

case_constantLine :: Assertion
case_constantLine = do
    let
        val = 7.0
        exName = TS.concat ["constantLine(", TS.pack $ show val, ")"]
        co = CounterSpec exName Nothing
        wc = WorldConstraints { wTimeRange = mkTimeRange 0 100, wMaxSamples = 11, wInterval = Nothing }

        Just ts' = M.lookup (co { csName = exName } ) $ timeSeriesMap $ constantLine wc val

        ex = mkExpectedTimeSeries exName 0 10 10 [7.0,7.0,7.0,7.0,7.0,7.0,7.0,7.0,7.0,7.0,7.0]

        avg = NonEmpty.map tscAvg . ptsData
        time = NonEmpty.map tscTime . ptsData

    assertEqual "Length should be 11" 11 (NonEmpty.length $ ptsData ts')
    assertEqual "Expected values computed" (avg ex) (avg ts')
    assertEqual "Expected times computed" (time ex) (time ts')
    assertEqual "Expected name is correct" exName (ptsName ts')

case_removeAbovePercentile :: Assertion
case_removeAbovePercentile = do
    let ts = mkExpectedTimeSeries tsName 0 10 10 [1,1,1,3,3,19,8,1,1,1,7]
        tsName = "a"
        co = CounterSpec tsName Nothing
        val = 95
        exName = TS.concat ["removeAbovePercentile(", TS.pack $ show val, ",", tsName, ")"]
        exCo = CounterSpec exName Nothing
        wc = WorldConstraints { wTimeRange = mkTimeRange 0 100, wMaxSamples = 11, wInterval = Nothing }

        Just ts' = M.lookup exCo $ timeSeriesMap $ removeAbovePercentile wc val $ def { timeSeriesMap = M.singleton co ts }

        ex = mkExpectedTimeSeries exName 0 10 10 [1,1,1,3,3,19,8,1,1,1,7]
        ex' = ex { ptsData = NonEmpty.fromList $ NonEmpty.filter (\x -> tscAvg x < 8) (ptsData ex) }

        avg = NonEmpty.map tscAvg . ptsData
        time = NonEmpty.map tscTime . ptsData

    assertEqual "Length should be 11" (NonEmpty.length $ ptsData ex') (NonEmpty.length $ ptsData ts')
    assertEqual "Expected values computed" (avg ex') (avg ts')
    assertEqual "Expected times computed" (time ex') (time ts')
    assertEqual "Expected name is correct" exName (ptsName ts')

case_removeBelowPercentile :: Assertion
case_removeBelowPercentile = do
    let ts = mkExpectedTimeSeries tsName 0 10 10 [1,1,1,3,3,19,8,1,1,1,7]
        tsName = "a"
        co = CounterSpec tsName Nothing
        val = 95
        exName = TS.concat ["removeBelowPercentile(", TS.pack $ show val, ",", tsName, ")"]
        exCo = CounterSpec exName Nothing
        wc = WorldConstraints { wTimeRange = mkTimeRange 0 100, wMaxSamples = 11, wInterval = Nothing }

        Just ts' = M.lookup exCo $ timeSeriesMap $ removeBelowPercentile wc val $ def { timeSeriesMap = M.singleton co ts }

        ex = mkExpectedTimeSeries exName 0 10 10 [1,1,1,3,3,19,8,1,1,1,7]
        ex' = ex { ptsData = NonEmpty.fromList $ NonEmpty.filter (\x -> tscAvg x > 8) (ptsData ex) }

        avg = NonEmpty.map tscAvg . ptsData
        time = NonEmpty.map tscTime . ptsData

    assertEqual "Length should be 11" (NonEmpty.length $ ptsData ex') (NonEmpty.length $ ptsData ts')
    assertEqual "Expected values computed" (avg ex') (avg ts')
    assertEqual "Expected times computed" (time ex') (time ts')
    assertEqual "Expected name is correct" exName (ptsName ts')

case_removeAbovePercentileInc :: Assertion
case_removeAbovePercentileInc = do
    let ts = mkExpectedTimeSeries tsName 0 10 10 [1,1,1,3,3,19,8,1,1,1,7]
        tsName = "a"
        co = CounterSpec tsName Nothing
        val = 95
        exName = TS.concat ["removeAbovePercentileInc(", TS.pack $ show val, ",", tsName, ")"]
        exCo = CounterSpec exName Nothing
        wc = WorldConstraints { wTimeRange = mkTimeRange 0 100, wMaxSamples = 11, wInterval = Nothing }

        Just ts' = M.lookup exCo $ timeSeriesMap $ removeAbovePercentileInc wc val $ def { timeSeriesMap = M.singleton co ts }

        ex = mkExpectedTimeSeries exName 0 10 10 [1,1,1,3,3,19,8,1,1,1,7]
        ex' = ex { ptsData = NonEmpty.fromList $ NonEmpty.filter (\x -> tscAvg x <= 8) (ptsData ex) }

        avg = NonEmpty.map tscAvg . ptsData
        time = NonEmpty.map tscTime . ptsData

    assertEqual "Length should be 11" (NonEmpty.length $ ptsData ex') (NonEmpty.length $ ptsData ts')
    assertEqual "Expected values computed" (avg ex') (avg ts')
    assertEqual "Expected times computed" (time ex') (time ts')
    assertEqual "Expected name is correct" exName (ptsName ts')

case_removeBelowPercentileInc :: Assertion
case_removeBelowPercentileInc = do
    let ts = mkExpectedTimeSeries tsName 0 10 10 [1,1,1,3,3,19,8,1,1,1,7]
        tsName = "a"
        co = CounterSpec tsName Nothing
        val = 95
        exName = TS.concat ["removeBelowPercentileInc(", TS.pack $ show val, ",", tsName, ")"]
        exCo = CounterSpec exName Nothing
        wc = WorldConstraints { wTimeRange = mkTimeRange 0 100, wMaxSamples = 11, wInterval = Nothing }

        Just ts' = M.lookup exCo $ timeSeriesMap $ removeBelowPercentileInc wc val $ def { timeSeriesMap = M.singleton co ts }

        ex = mkExpectedTimeSeries exName 0 10 10 [1,1,1,3,3,19,8,1,1,1,7]
        ex' = ex { ptsData = NonEmpty.fromList $ NonEmpty.filter (\x -> tscAvg x >= 8) (ptsData ex) }

        avg = NonEmpty.map tscAvg . ptsData
        time = NonEmpty.map tscTime . ptsData

    assertEqual "Length should be 11" (NonEmpty.length $ ptsData ex') (NonEmpty.length $ ptsData ts')
    assertEqual "Expected values computed" (avg ex') (avg ts')
    assertEqual "Expected times computed" (time ex') (time ts')
    assertEqual "Expected name is correct" exName (ptsName ts')

case_keepTimeSeriesWithin2StdDeviations :: Assertion
case_keepTimeSeriesWithin2StdDeviations = do
    let ts = mkExpectedTimeSeries tsName 0 10 10 [1,2,1,2,17,1,2,1,2,1,2]
        ts2 = mkExpectedTimeSeries tsName2 0 10 10 [1.1,1,1.1,1,1.1,1,1.1,1,1.1,1,1]
        v = 2
        tsName = "a"
        tsName2 = "b"
        co = CounterSpec tsName Nothing
        co2 = CounterSpec tsName2 Nothing
        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        tsM = timeSeriesMap $ keepSeriesWithinDeviation wc v $ def { timeSeriesMap = M.fromList [(co,ts), (co2,ts2)] }
        Just ts' = M.lookup co2 tsM

    assertEqual "Number of series" 1 (M.size tsM)
    assertEqual "Expected name is correct" tsName2 (ptsName ts')

case_keepTimeSeriesAbove2StdDeviations :: Assertion
case_keepTimeSeriesAbove2StdDeviations = do
    let ts = mkExpectedTimeSeries tsName 0 10 10 [1,2,1,2,17,1,2,1,2,1,2]
        ts2 = mkExpectedTimeSeries tsName2 0 10 10 [1,2,1,2,1,2,1,2,1,2,1]
        v = 2
        tsName = "a"
        tsName2 = "b"
        co = CounterSpec tsName Nothing
        co2 = CounterSpec tsName2 Nothing
        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        tsM = timeSeriesMap $ keepSeriesAboveDeviation wc v $ def { timeSeriesMap = M.fromList [(co,ts), (co2,ts2)] }
        Just ts' = M.lookup co tsM

    assertEqual "Number of series" 1 (M.size tsM)
    assertEqual "Expected name is correct" tsName (ptsName ts')

case_sumOffsetRanges :: Assertion
case_sumOffsetRanges = do
    let tsName = "ibab.imvu.xy_num_connected_app_users.v2.mobile.ibab-manager"
        defStart = 0
        realStart = 7
        interval = 10
        realEnd = 10
        ts = mkExpectedTimeSeries tsName defStart realEnd interval [0,0,0,0,0,0,0,1,2,3,4]
        ts' = ts { ptsData = NonEmpty.fromList $ NonEmpty.drop realStart (ptsData ts), ptsRange = TimeRangeE { treStart = TimeWrapper $ secondsToUTC (realStart * interval ), treEnd = TimeWrapper $ secondsToUTC (realEnd * interval) } }

        co = CounterSpec tsName Nothing

        constName = "constantLine(1000.0)"
        tsConst = mkExpectedTimeSeries constName defStart realEnd interval [10,10,10,10,10,10,10,10,10,10,10]
        co2 = CounterSpec constName Nothing

        exName = TS.concat ["sum(", constName, ",", tsName, ")"]
        ex = mkExpectedTimeSeries exName defStart realEnd interval [10,10,10,10,10,10,10,11,12,13,14]
        exCo = CounterSpec exName Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        tsM = timeSeriesMap $ sumify wc $ def { timeSeriesMap = M.fromList [(co,ts'), (co2,tsConst)] }
        Just res = M.lookup exCo tsM

        avg = NonEmpty.map tscAvg . ptsData
        time = NonEmpty.map tscTime . ptsData


    assertEqual "TimeSeries averages equal to expected" (avg ex) (avg res)
    assertEqual "Time valies are expected " (time ex) (time res)
    assertEqual "Outer Range expected" (ptsRange ex) (ptsRange res)

case_sumGapRanges :: Assertion
case_sumGapRanges = do
    let tsName = "gaps1"
        tsName2 = "gaps2"
        interval = 10 :: Int
        ts = mkExpectedTimeSeries tsName 0 10 10 [1,2,0,0,5,6,0,0,9,10,11]
        ts2 = mkExpectedTimeSeries tsName 0 10 10 [0,2,3,4,5,0,0,8,9,10,0]
        ts' = ts { ptsData = NonEmpty.fromList $ NonEmpty.filter ((/=0) . tscAvg) (ptsData ts)}
        ts2' = ts2 { ptsData = NonEmpty.fromList $ NonEmpty.filter ((/=0) . tscAvg) (ptsData ts2), ptsRange = TimeRangeE { treStart = TimeWrapper $ secondsToUTC (1 * interval ), treEnd = TimeWrapper $ secondsToUTC (9 * interval) } }

        co = CounterSpec tsName Nothing
        co2 = CounterSpec tsName2 Nothing

        exName = TS.concat ["sum(", tsName, ",", tsName2, ")"]
        ex = mkExpectedTimeSeries exName 0 10 10 [1,4,3,4,10,6,0,8,18,20,11]
        ex' = ex { ptsData = NonEmpty.fromList $ NonEmpty.filter ((/=0) . tscAvg) (ptsData ex)}
        exCo = CounterSpec exName Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        tsM = timeSeriesMap $ sumify wc $ def { timeSeriesMap = M.fromList [(co,ts'), (co2,ts2')] }
        Just res = M.lookup exCo tsM

        avg = NonEmpty.map tscAvg . ptsData
        time = NonEmpty.map tscTime . ptsData


    assertEqual "TimeSeries averages equal to expected" (avg ex') (avg res)
    assertEqual "Time valies are expected " (time ex') (time res)
    assertEqual "Outer Range expected" (ptsRange ex') (ptsRange res)

case_exclude :: Assertion
case_exclude = do
    let tsName = "gaps1"
        tsName2 = "gaps2"
        tsName3 = "empty"
        ts = mkExpectedTimeSeries tsName 0 10 10 [1,2,0,0,5,6,0,0,9,10,11]
        ts2 = mkExpectedTimeSeries tsName2 0 10 10 [0,2,3,4,5,0,0,8,9,10,0]
        ts3 = mkExpectedTimeSeries tsName3 0 10 10 [0,0,0,0,0,0,0,0,0,0]
        co = CounterSpec tsName Nothing
        co2 = CounterSpec tsName2 Nothing
        co3 = CounterSpec tsName3 Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        excludeText = "g[a]ps"
        origMap = def { timeSeriesMap =  M.fromList [(co,ts), (co2,ts2), (co3,ts3)] }
        expectedMap = M.fromList [(co3,ts3)]

        tsM = timeSeriesMap $ exclude wc excludeText origMap

    assertEqual "All matching exclude were dropped" expectedMap tsM

case_include :: Assertion
case_include = do
    let tsName = "gaps1"
        tsName2 = "gaps2"
        tsName3 = "empty"
        ts = mkExpectedTimeSeries tsName 0 10 10 [1,2,0,0,5,6,0,0,9,10,11]
        ts2 = mkExpectedTimeSeries tsName2 0 10 10 [0,2,3,4,5,0,0,8,9,10,0]
        ts3 = mkExpectedTimeSeries tsName3 0 10 10 [0,0,0,0,0,0,0,0,0,0]
        co = CounterSpec tsName Nothing
        co2 = CounterSpec tsName2 Nothing
        co3 = CounterSpec tsName3 Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        includeText = "g[a]ps"
        origMap = def { timeSeriesMap =  M.fromList [(co,ts), (co2,ts2), (co3,ts3)] }
        expectedMap = M.fromList [(co,ts),(co2,ts2)]

        tsM = timeSeriesMap $ include wc includeText origMap

    assertEqual "All matching include were kept" expectedMap tsM

case_keepAbove :: Assertion
case_keepAbove = do
    let tsName = "errors"
        tsName2 = "other_errors"
        tsName3 = "empty"
        ts = mkExpectedTimeSeries tsName 0 10 10 [1,2,0,0,5,6,0,0,9,10,11]
        ts2 = mkExpectedTimeSeries tsName2 0 10 10 [0,2,3,4,5,0,0,8,9,10,0]
        ts3 = mkExpectedTimeSeries tsName3 0 10 10 [0,0,0,0,0,0,0,0,0,0]
        co = CounterSpec tsName Nothing
        co2 = CounterSpec tsName2 Nothing
        co3 = CounterSpec tsName3 Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap =  M.fromList [(co,ts), (co2,ts2), (co3,ts3)] }
        expectedMap = M.fromList [(co,ts)]

        tsM = timeSeriesMap $ keepSeriesAbove wc 10.0 origMap

    assertEqual "Keep series with a value of more than 10 at any point in timerange" expectedMap tsM

case_keepAboveInc :: Assertion
case_keepAboveInc = do
    let tsName = "errors"
        tsName2 = "other_errors"
        tsName3 = "empty"
        ts = mkExpectedTimeSeries tsName 0 10 10 [1,2,0,0,5,6,0,0,9,10,11]
        ts2 = mkExpectedTimeSeries tsName2 0 10 10 [0,2,3,4,5,0,0,8,9,10,0]
        ts3 = mkExpectedTimeSeries tsName3 0 10 10 [0,0,0,0,0,0,0,0,0,0]
        co = CounterSpec tsName Nothing
        co2 = CounterSpec tsName2 Nothing
        co3 = CounterSpec tsName3 Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap =  M.fromList [(co,ts), (co2,ts2), (co3,ts3)] }
        expectedMap = M.fromList [(co,ts), (co2, ts2)]

        tsM = timeSeriesMap $ keepSeriesAboveInc wc 10.0 origMap

    assertEqual "Keep series with a value of more than or equal to 10 at any point in timerange" expectedMap tsM

case_keepAllAbove :: Assertion
case_keepAllAbove = do
    let tsName = "errors"
        tsName2 = "other_errors"
        tsName3 = "empty"
        ts = mkExpectedTimeSeries tsName 0 10 10 [11,12,11,11,15,16,11,11,19,20,21]
        ts2 = mkExpectedTimeSeries tsName2 0 10 10 [11,9,11,11,15,16,11,11,19,20,21]
        ts3 = mkExpectedTimeSeries tsName3 0 10 10 [0,0,0,0,0,0,0,0,0,0]
        co = CounterSpec tsName Nothing
        co2 = CounterSpec tsName2 Nothing
        co3 = CounterSpec tsName3 Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap =  M.fromList [(co,ts), (co2,ts2), (co3,ts3)] }
        expectedMap = M.fromList [(co,ts)]

        tsM = timeSeriesMap $ keepSeriesAllAbove wc 10.0 origMap

    assertEqual "Keep series where all values are more than 10 in timerange" expectedMap tsM

case_removeAboveValue :: Assertion
case_removeAboveValue = do
    let ts = mkExpectedTimeSeries tsName 0 10 10 [1,1,1,3,3,19,8,1,1,1,7]
        tsName = "a"
        co = CounterSpec tsName Nothing
        val = 8
        exName = TS.concat ["removeAboveValue(", TS.pack $ show val, ",", tsName, ")"]
        exCo = CounterSpec exName Nothing
        wc = WorldConstraints { wTimeRange = mkTimeRange 0 100, wMaxSamples = 11, wInterval = Nothing }

        Just ts' = M.lookup exCo $ timeSeriesMap $ removeAboveValue wc val $ def { timeSeriesMap = M.singleton co ts }

        ex = mkExpectedTimeSeries exName 0 10 10 [1,1,1,3,3,19,8,1,1,1,7]
        ex' = ex { ptsData = NonEmpty.fromList $ NonEmpty.filter (\x -> tscAvg x < val) (ptsData ex) }

        avg = NonEmpty.map tscAvg . ptsData
        time = NonEmpty.map tscTime . ptsData

    assertEqual "Length should be 11" (NonEmpty.length $ ptsData ex') (NonEmpty.length $ ptsData ts')
    assertEqual "Expected values computed" (avg ex') (avg ts')
    assertEqual "Expected times computed" (time ex') (time ts')
    assertEqual "Expected name is correct" exName (ptsName ts')

case_maxAtSamples :: Assertion
case_maxAtSamples = do
    let tsName = "errors"
        tsName2 = "other_errors"
        tsName3 = "empty"
        ts = mkExpectedTimeSeries tsName 0 10 10 [11,12,11,11,15,16,11,11,22,20,21]
        ts2 = mkExpectedTimeSeries tsName2 0 10 10 [11,9,11,11,15,19,11,14,19,20,21]
        ts3 = mkExpectedTimeSeries tsName3 0 10 10 [0,0,0,0,0,0,0,0,33,0,1]
        co = CounterSpec tsName Nothing
        co2 = CounterSpec tsName2 Nothing
        co3 = CounterSpec tsName3 Nothing

        exName = TS.concat ["maxAtSamplesAvg(", tsName3, ",", tsName, ",", tsName2, ")"]
        exS = mkExpectedTimeSeries exName 0 10 10 [11,12,11,11,15,19,11,14,33,20,21]
        coEx = CounterSpec exName Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap =  M.fromList [(co,ts), (co2,ts2), (co3,ts3)] }
        expectedMap = M.fromList [(coEx,exS)]

        tsM = timeSeriesMap $ maxAtSamples wc ("Avg", tscAvg) origMap

    assertEqual "Keep the max value at each sample" expectedMap tsM

case_summarizeAvg :: Assertion
case_summarizeAvg = do
    let tsName = "errors"
        interval = 10
        ts = mkExpectedTimeSeries tsName 0 35 interval  [10,12,14,16,18,20
                                                        ,20,22,24,26,28,30
                                                        ,30,32,34,36,38,40
                                                        ,40,42,44,46,48,50
                                                        ,50,52,54,56,58,60
                                                        ,60,62,64,66,68,70]
        co = CounterSpec tsName Nothing

        targetInterval = 60
        exName = TS.concat ["summarize(", (TS.pack . show) targetInterval, ",", tsName, ")"]
        exS = mkExpectedTimeSeries exName 0 6 targetInterval [15,25,35,45,55,65]
        coEx = CounterSpec exName Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap =  M.fromList [(co,ts)] }
        expectedMap = M.fromList [(coEx,exS)]

        tsM = timeSeriesMap $ summarize AvgSummarizer wc targetInterval origMap

    assertEqual "Summarized values to interval 60 with avg" (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx expectedMap) ) (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx tsM) )

case_summarizeAvgSameInterval :: Assertion
case_summarizeAvgSameInterval = do
    let tsName = "errors"
        interval = 10
        values = [10,12,14,16,18,20
                ,20,22,24,26,28,30
                ,30,32,34,36,38,40
                ,40,42,44,46,48,50
                ,50,52,54,56,58,60
                ,60,62,64,66,68,70]
        ts = mkExpectedTimeSeries tsName 0 35 interval values
        co = CounterSpec tsName Nothing

        targetInterval = 10
        exName = TS.concat ["summarize(", (TS.pack . show) targetInterval, ",", tsName, ")"]
        exS = mkExpectedTimeSeries exName 0 35 targetInterval values
        coEx = CounterSpec exName Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap =  M.fromList [(co,ts)] }
        expectedMap = M.fromList [(coEx,exS)]

        tsM = timeSeriesMap $ summarize AvgSummarizer wc targetInterval origMap

    assertEqual "Summarizer with same interval is noop" (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx expectedMap) ) (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx tsM) )

case_summarizeAvgOddInterval :: Assertion
case_summarizeAvgOddInterval = do
    let tsName = "errors"
        interval = 10
        values = [10,12,14,16,18,20
                ,20,22,24,26,28,30
                ,30,32,34,36,38,40
                ,40,42,44,46,48,50
                ,50,52,54,56,58,60
                ,60,62,64,66,68,70]
        ts = mkExpectedTimeSeries tsName 0 36 interval values
        co = CounterSpec tsName Nothing

        targetInterval = 15
        exName = TS.concat ["summarize(", (TS.pack . show) targetInterval, ",", tsName, ")"]
        exS = mkExpectedTimeSeries exName 0 24 targetInterval [11,14
                                                              ,17,20
                                                              ,21,24
                                                              ,27,30
                                                              ,31,34
                                                              ,37,40
                                                              ,41,44
                                                              ,47,50
                                                              ,51,54
                                                              ,57,60
                                                              ,61,64
                                                              ,67,70]
        coEx = CounterSpec exName Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap =  M.fromList [(co,ts)] }
        expectedMap = M.fromList [(coEx,exS)]

        tsM = timeSeriesMap $ summarize AvgSummarizer wc targetInterval origMap

    assertEqual "Summarizer with odd interval is handled" (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx expectedMap) ) (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx tsM) )

case_summarizeSum :: Assertion
case_summarizeSum = do
    let tsName = "errors"
        interval = 10
        ts = mkExpectedTimeSeries tsName 0 35 interval  [10,12,14,16,18,20
                                                        ,20,22,24,26,28,30
                                                        ,30,32,34,36,38,40
                                                        ,40,42,44,46,48,50
                                                        ,50,52,54,56,58,60
                                                        ,60,62,64,66,68,70]
        co = CounterSpec tsName Nothing

        targetInterval = 60
        exName = TS.concat ["summarize(", (TS.pack . show) targetInterval, ",", tsName, ")"]
        exS = mkExpectedTimeSeries exName 0 6 targetInterval [90,150,210,270,330,390]
        coEx = CounterSpec exName Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap =  M.fromList [(co,ts)] }
        expectedMap = M.fromList [(coEx,exS)]

        tsM = timeSeriesMap $ summarize SumSummarizer wc targetInterval origMap

    assertEqual "Summarized values to interval 60 with avg" (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx expectedMap) ) (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx tsM) )

case_summarizeSumSameInterval :: Assertion
case_summarizeSumSameInterval = do
    let tsName = "errors"
        interval = 10
        values = [10,12,14,16,18,20
                ,20,22,24,26,28,30
                ,30,32,34,36,38,40
                ,40,42,44,46,48,50
                ,50,52,54,56,58,60
                ,60,62,64,66,68,70]
        ts = mkExpectedTimeSeries tsName 0 35 interval values
        co = CounterSpec tsName Nothing

        targetInterval = 10
        exName = TS.concat ["summarize(", (TS.pack . show) targetInterval, ",", tsName, ")"]
        exS = mkExpectedTimeSeries exName 0 35 targetInterval values
        coEx = CounterSpec exName Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap =  M.fromList [(co,ts)] }
        expectedMap = M.fromList [(coEx,exS)]

        tsM = timeSeriesMap $ summarize SumSummarizer wc targetInterval origMap

    assertEqual "Summarizer with same interval is noop" (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx expectedMap) ) (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx tsM) )

case_summarizeSumOddInterval :: Assertion
case_summarizeSumOddInterval = do
    let tsName = "errors"
        interval = 10
        values = [10,12,14,16,18,20
                ,20,22,24,26,28,30
                ,30,32,34,36,38,40
                ,40,42,44,46,48,50
                ,50,52,54,56,58,60
                ,60,62,64,66,68,70]
        ts = mkExpectedTimeSeries tsName 0 36 interval values
        co = CounterSpec tsName Nothing

        targetInterval = 15
        exName = TS.concat ["summarize(", (TS.pack . show) targetInterval, ",", tsName, ")"]
        exS = mkExpectedTimeSeries exName 0 24 targetInterval [22,14
                                                              ,34,20
                                                              ,42,24
                                                              ,54,30
                                                              ,62,34
                                                              ,74,40
                                                              ,82,44
                                                              ,94,50
                                                              ,102,54
                                                              ,114,60
                                                              ,122,64
                                                              ,134,70]
        coEx = CounterSpec exName Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap =  M.fromList [(co,ts)] }
        expectedMap = M.fromList [(coEx,exS)]

        tsM = timeSeriesMap $ summarize SumSummarizer wc targetInterval origMap

    assertEqual "Summarizer with odd interval is handled" (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx expectedMap) ) (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx tsM) )

case_offset :: Assertion
case_offset = do
    let tsName = "errors"
        interval = 10
        ts = mkExpectedTimeSeries tsName 0 10 interval [11,12,11,11,15,16,11,11,22,20,21]
        co = CounterSpec tsName Nothing

        offsetVal = 100.0
        exName = TS.concat ["offset(", (TS.pack . show) offsetVal, ",", tsName, ")"]
        exS = mkExpectedTimeSeries exName 0 10 10 [111,112,111,111,115,116,111,111,122,120,121]
        coEx = CounterSpec exName Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap =  M.fromList [(co,ts)] }
        expectedMap = M.fromList [(coEx,exS)]

        tsM = timeSeriesMap $ offset wc offsetVal origMap

    assertEqual "Offset each value in the map" (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx expectedMap) ) (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx tsM) )

case_offsetNeg :: Assertion
case_offsetNeg = do
    let tsName = "errors"
        interval = 10
        ts = mkExpectedTimeSeries tsName 0 10 interval [11,12,11,11,15,16,11,11,22,20,21]
        co = CounterSpec tsName Nothing

        offsetVal = -100.0
        exName = TS.concat ["offset(", (TS.pack . show) offsetVal, ",", tsName, ")"]
        exS = mkExpectedTimeSeries exName 0 10 10 [-89,-88,-89,-89,-85,-84,-89,-89,-78,-80,-79]
        coEx = CounterSpec exName Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap =  M.fromList [(co,ts)] }
        expectedMap = M.fromList [(coEx,exS)]

        tsM = timeSeriesMap $ offset wc offsetVal origMap

    assertEqual "Offset each value in the map" (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx expectedMap) ) (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx tsM) )

case_derive :: Assertion
case_derive = do
    let tsName = "errors"
        interval = 10
        ts = mkExpectedTimeSeries tsName 0 10 interval [11,12,11,11,15,16,11,11,22,20,21]
        co = CounterSpec tsName Nothing

        exName = TS.concat ["derive(", tsName, ")"]
        exS = mkExpectedTimeSeries exName 0 10 10 [0,0.1,-0.1,0,0.4,0.1,-0.5,0,1.1,-0.2,0.1]
        coEx = CounterSpec exName Nothing

        wc = WorldConstraints { wTimeRange = mkTimeRange 0 10, wMaxSamples = 11, wInterval = Just (10 :: Int) }

        origMap = def { timeSeriesMap = M.fromList [(co,ts)] }
        expectedMap = M.fromList [(coEx,exS)]

        tsM = timeSeriesMap $ derivify wc origMap

    assertEqual "derive" (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx expectedMap) ) (fmap (NonEmpty.map tscAvg . ptsData) (M.lookup coEx tsM) )

case_correctTimeSeriesIntervals :: Assertion
case_correctTimeSeriesIntervals = do
    let pairs = NonEmpty.fromList [(def { tscAvg = 1, tscTime = 300}, def { tscAvg = 2, tscTime=600}),(def { tscAvg = 2, tscTime=600}, def {tscAvg = 4, tscTime=900})]
        ixs = NonEmpty.fromList [(0,0.0),(0,0.4),(0,0.8),(1,0.2),(1,0.6),(2,0.0)]
        Just res = magic (Just pairs) (Just ixs)
        res' = rekey res 120

    let expected = NonEmpty.fromList
                  [ TimeSeriesChunk {
                          tscTime = 300
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 1.0
                        , tscSdev = 0.0},
                    TimeSeriesChunk {
                          tscTime = 420
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 1.4
                        , tscSdev = 0.0},
                    TimeSeriesChunk {
                          tscTime = 540
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 1.8
                        , tscSdev = 0.0},
                    TimeSeriesChunk {
                          tscTime = 660
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 2.4
                        , tscSdev = 0.0},
                    TimeSeriesChunk {
                          tscTime = 780
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 3.2
                        , tscSdev = 0.0}]
    assertEqual "Intervals are corrected" expected res'

case_correctTimeSeriesIntervalsBugOne :: Assertion
case_correctTimeSeriesIntervalsBugOne = do
    let tsD = TimeSeriesN { tsnRange = def, tsnInterval = 300, tsnData = NonEmpty.fromList [def { tscAvg = 1, tscTime = 340}] }
        Just res = correctTimeSeriesIntervals 300 (Just def {tscAvg = 10, tscTime = 340 }) tsD

    let expected = NonEmpty.fromList [TimeSeriesChunk {
                          tscTime = 340
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 1.0
                        , tscSdev = 0.0}
                        ]

    assertEqual "Time series is returned unchanged" expected (tsnData res)

case_correctTimeSeriesIntervalsBugBackward :: Assertion
case_correctTimeSeriesIntervalsBugBackward = do
    let tsD = TimeSeriesN { tsnRange = def, tsnInterval = 300, tsnData = NonEmpty.fromList [def { tscAvg = 1, tscTime = 340}, def { tscAvg = 2, tscTime=640}, def {tscAvg = 4, tscTime=940}, def { tscAvg = 10, tscTime = 1240 }] }
        Just res = correctTimeSeriesIntervals 300 (Just def {tscAvg = 10, tscTime = 100 }) tsD

    let expected = NonEmpty.fromList [TimeSeriesChunk {
                          tscTime = 100
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 1.0
                        , tscSdev = 0.0},
                    TimeSeriesChunk {
                          tscTime = 400
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 1.2
                        , tscSdev = 0.0},
                    TimeSeriesChunk {
                          tscTime = 700
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 2.4
                        , tscSdev = 0.0},
                    TimeSeriesChunk {
                          tscTime = 1000
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 5.199999999999999
                        , tscSdev = 0.0}]
    assertEqual "Intervals are corrected" expected (tsnData res)

case_correctTimeSeriesIntervalsBugGoodModuloWith0Buckets :: Assertion
case_correctTimeSeriesIntervalsBugGoodModuloWith0Buckets = do
    let tsD = TimeSeriesN { tsnRange = def, tsnInterval = 60, tsnData = NonEmpty.fromList [def, def { tscAvg = 1, tscTime = 120}, def { tscAvg = 2, tscTime=180}, def {tscAvg = 4, tscTime=240}, def { tscAvg = 10, tscTime = 300 }] }
        Just res = correctTimeSeriesIntervals 60 (Just def {tscAvg = 10, tscTime = 60 }) tsD

    let expected = NonEmpty.fromList [  def
                    , TimeSeriesChunk {
                          tscTime = 120
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 1.0
                        , tscSdev = 0.0}
                    , TimeSeriesChunk {
                          tscTime = 180
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 2.0
                        , tscSdev = 0.0}
                    , TimeSeriesChunk {
                          tscTime = 240
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 4.0
                        , tscSdev = 0.0}
                    , TimeSeriesChunk {
                          tscTime = 300
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 10
                        , tscSdev = 0.0}]
    assertEqual "Intervals are corrected" expected (tsnData res)

case_correctTimeSeriesIntervalsBugForward :: Assertion
case_correctTimeSeriesIntervalsBugForward = do
    let tsD = TimeSeriesN { tsnRange = def, tsnInterval = 300, tsnData = NonEmpty.fromList [def { tscAvg = 1, tscTime = 40}, def { tscAvg = 2, tscTime=340}, def {tscAvg = 4, tscTime=640}, def { tscAvg = 10, tscTime = 940 }] }
        Just res = correctTimeSeriesIntervals 300 (Just def {tscAvg = 10, tscTime = 100 }) tsD

    let expected = NonEmpty.fromList [TimeSeriesChunk {
                          tscTime = 100
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 1.2
                        , tscSdev = 0.0},
                    TimeSeriesChunk {
                          tscTime = 400
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 2.4
                        , tscSdev = 0.0},
                    TimeSeriesChunk {
                          tscTime = 700
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 5.2
                        , tscSdev = 0.0}]
    assertEqual "Intervals are corrected" expected (tsnData res)

--case_correctTimeSeriesIntervalsBugHandlesEmpty = do
--    let tsD = TimeSeriesN { tsnRange = def, tsnInterval = 300, tsnData = [] }
--        res = correctTimeSeriesIntervals 300 (Just def {tscAvg = 10, tscTime = 100 }) tsD
--
--    let expected = []
--    assertEqual "Intervals are corrected" expected (tsnData res)

case_correctIntervalsBugHandlesStartingWithNoData :: IO ()
case_correctIntervalsBugHandlesStartingWithNoData = do
    let tsD = TimeSeriesN { tsnRange = def, tsnInterval = 60, tsnData = NonEmpty.fromList [def {tscTime = 0}, def { tscAvg = 1, tscTime = 120}, def { tscAvg = 2, tscTime=180}, def {tscAvg = 4, tscTime=240}, def { tscAvg = 10, tscTime = 300 }] }
        tsD2 = TimeSeriesN { tsnRange = def, tsnInterval = 60, tsnData = NonEmpty.fromList [def {tscTime = 0}, def { tscTime = 0}, def { tscAvg = 2, tscTime=180}, def {tscAvg = 4, tscTime=240}, def { tscAvg = 10, tscTime = 300 }] }
        pr = PostReplyN { prnKeys = M.fromList [("a", tsD),("b", tsD2)], prnRange = def, prnInterval = 60 }
        (Nothing,res):_ = correctIntervals [(Nothing, pr)]

        Just tsDR = tsnData <$> M.lookup "a" (prnKeys res)
        Just tsD2R = tsnData <$> M.lookup "b" (prnKeys res)

    assertEqual "Intervals are left as they are" (tsnData tsD) tsDR
    assertEqual "Intervals are left as they are" (tsnData tsD2) tsD2R

case_timeZeriesZips :: IO ()
case_timeZeriesZips = do
    let testdata = NonEmpty.fromList $ map (\x -> def { tscTime = x, tscAvg = fromIntegral x}) [10,20..100]
        testdata2 = NonEmpty.fromList $ map (\x -> def { tscTime = x, tscAvg = fromIntegral x}) [10,20..100]
        result = timeSeriesZip addTimeSeriesChunk testdata testdata2
        result2 = timeSeriesZip addTimeSeriesChunk testdata2 testdata

        expected = NonEmpty.fromList [ TimeSeriesChunk
                        { tscTime = 10
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 20.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 20
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 40.0
                        , tscSdev = 0.0}
                    , TimeSeriesChunk
                        { tscTime = 30
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 60.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 40
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 80.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 50
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 100.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 60
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 120.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 70
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 140.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 80
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 160.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 90
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 180.0
                        , tscSdev = 0.0}
                    , TimeSeriesChunk
                        { tscTime = 100
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 200.0
                        , tscSdev = 0.0}
                   ]

    assertEqual "Properly zipped" expected result
    assertEqual "Properly zipped other order" expected result2

case_timeZeriesZipsUneven :: IO ()
case_timeZeriesZipsUneven = do
    let testdata = NonEmpty.fromList $ map (\x -> def { tscTime = x, tscAvg = fromIntegral x}) [10,20..100]
        testdata2 = NonEmpty.fromList $ map (\x -> def { tscTime = x, tscAvg = fromIntegral x}) [10,20..40]
        result = timeSeriesZip addTimeSeriesChunk testdata testdata2
        result2 = timeSeriesZip addTimeSeriesChunk testdata2 testdata

        expected = NonEmpty.fromList [ TimeSeriesChunk
                        { tscTime = 10
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 20.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 20
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 40.0
                        , tscSdev = 0.0}
                    , TimeSeriesChunk
                        { tscTime = 30
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 60.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 40
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 80.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 50
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 50.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 60
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 60.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 70
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 70.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 80
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 80.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 90
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 90.0
                        , tscSdev = 0.0}
                    , TimeSeriesChunk
                        { tscTime = 100
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 100.0
                        , tscSdev = 0.0}
                   ]

    assertEqual "Properly zipped" expected result
    assertEqual "Properly zipped other order" expected result2

case_timeZeriesZipsGaps :: IO ()
case_timeZeriesZipsGaps = do
    let testdata = NonEmpty.fromList $ filter (\x -> notElem (tscTime x) [50,60,90] ) $ map (\x -> def { tscTime = x, tscAvg = fromIntegral x}) [10,20..100]
        testdata2 = NonEmpty.fromList $ filter (\x -> notElem (tscTime x) [40]) $ map (\x -> def { tscTime = x, tscAvg = fromIntegral x}) [10,20..70]
        result = timeSeriesZip addTimeSeriesChunk testdata testdata2
        result2 = timeSeriesZip addTimeSeriesChunk testdata2 testdata

        expected = NonEmpty.fromList [ TimeSeriesChunk
                        { tscTime = 10
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 20.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 20
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 40.0
                        , tscSdev = 0.0}
                    , TimeSeriesChunk
                        { tscTime = 30
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 60.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 40
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 40.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 50
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 50.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 60
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 60.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 70
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 140.0
                        , tscSdev = 0.0}
                   , TimeSeriesChunk
                        { tscTime = 80
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 80.0
                        , tscSdev = 0.0}
                    , TimeSeriesChunk
                        { tscTime = 100
                        , tscCount = 0
                        , tscMin = 0.0
                        , tscMax = 0.0
                        , tscSum = 0.0
                        , tscSumsq = 0.0
                        , tscAvg = 100.0
                        , tscSdev = 0.0}
                   ]

    assertEqual "Properly zipped" expected result
    assertEqual "Properly zipped other order" expected result2

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
