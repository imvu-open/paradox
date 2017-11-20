{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Paradox.Functions.Math (
  summarize
, offset
, removeBelowPercentile
, removeAbovePercentile
, removeBelowPercentileInc
, removeAbovePercentileInc
, removeBelowValue
, removeAboveValue
, removeBelowValueInc
, removeAboveValueInc
, ceiling
, floor
, medianFilter
, movingAverage
, scale
, scalarDivide
, scaleByInterval
, divideByInterval
, averageTimeSeries
, sumify
, graphField
, maxAtSamples
, integrify
, derivify
, logP
, absTimeSeries
, subtractify
, divideTimeSeries
, medianAbsoluteDeviation
, medianAbsoluteDeviationFilter
, doubleMedianAbsoluteDeviation
, doubleMedianAbsoluteDeviationFilter
, loess
, edmMulti
, stl
, stlSeason
, stlTrend
, stlResidual
) where

import Prelude hiding                           ( foldl1
                                                , foldl
                                                , foldr
                                                , foldr1
                                                , ceiling
                                                , floor
                                                )

import qualified Prelude (ceiling)

import Data.Default

import Language.Paradox.Eval.Types

import Data.List.NonEmpty                       (NonEmpty)
import Data.Foldable                            ( foldr1
                                                , foldr
                                                )
import Data.Traversable                         (mapAccumL)
import Data.Maybe                               (mapMaybe)
import Paradox.Istatd.Types                     ( TimeSeriesChunk(..)
                                                , ParadoxTimeSeries(..)
                                                , TSMap
                                                , CounterSpec(..)
                                                , ReturnData
                                                , ParadoxReturn(..)
                                                )
import Data.Monoid                              ( (<>) )
import Paradox.Istatd.Functions                 (zeroTSC)
import Paradox.Functions.Util                   ( Direction(..)
                                                , Summarizer(..)
                                                , directionComparison
                                                , toInfix
                                                , toPostfix
                                                , tails
                                                )
import Paradox.Functions.Math.TimeSeries        ( addTS
                                                , subTS
                                                , divTS
                                                , divTSScalar
                                                , cmpGTTS
                                                )
import Paradox.Functions.Math.TimeSeriesChunk   ( divTimeSeriesChunkScalar
                                                , mulTimeSeriesChunkScalar
                                                , sortTSByAvg
                                                , getPercentile
                                                , addTimeSeriesChunk
                                                , subTimeSeriesChunk
                                                , divTimeSeriesChunk
                                                , offsetTimeSeriesChunk
                                                )

import qualified Paradox.Istatd.Types           as IT
import qualified Paradox.Anomaly                as Anomaly
import qualified Data.Map.Strict                as M
import qualified Data.List.Split                as LS
import qualified Data.Text                      as TS
import qualified Data.List.NonEmpty             as NonEmpty

sumSummarize :: NonEmpty TimeSeriesChunk
             -> TimeSeriesChunk
sumSummarize = foldr1 addTimeSeriesChunk

avgSummarize :: NonEmpty TimeSeriesChunk
             -> TimeSeriesChunk
avgSummarize a =
    let d = fromIntegral $ NonEmpty.length a
        sumd = foldr1 addTimeSeriesChunk a
    in divTimeSeriesChunkScalar d sumd

sumarizer :: Summarizer
          -> NonEmpty TimeSeriesChunk
          -> TimeSeriesChunk
sumarizer = \case
    SumSummarizer -> sumSummarize
    AvgSummarizer -> avgSummarize

summarize' :: Summarizer
           -> Int
           -> CounterSpec
           -> ParadoxTimeSeries
           -> Maybe ParadoxTimeSeries
summarize' sm target cs pts = do
    let chunked :: [NonEmpty TimeSeriesChunk]
        chunked = mapMaybe NonEmpty.nonEmpty $ LS.splitPlaces places $ NonEmpty.toList $ ptsData pts

        factor = fromIntegral target
        startTime = tscTime $ NonEmpty.head (ptsData pts)
        places :: [Int]
        (places,_,_) = foldr (\i (a,ai,c) ->
                                if i /= ai
                                   then (c:a, i, 1)
                                   else (a,ai,c+1)
                             )
                             ([],0,0)
                             indices
        indices = (-1):foldr (\x a ->
                                (tscTime x - startTime) `div` factor : a
                             )
                             []
                             (ptsData pts)

    ptsData <- NonEmpty.nonEmpty $ map (sumarizer sm) chunked

    return $ pts { ptsData = ptsData
                 , ptsName = (TS.pack . show ) cs
                 }

summarize :: Summarizer
          -> WorldConstraints
          -> Int
          -> ReturnData
          -> ReturnData
summarize sm _w target pt@ParadoxReturn { timeSeriesMap = istatdData} =
    pt { timeSeriesMap = go sm target istatdData }
    where
        go sm' target' istatdData' =
            M.mapMaybeWithKey (summarize' sm' target')
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "summarize"
                                       , "("
                                       , (TS.pack . show) target
                                       , ","
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'

offset :: WorldConstraints
       -> Double
       -> ReturnData
       -> ReturnData
offset _w offsetVal pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go offsetVal istatdData }
    where
        go offsetVal' istatdData' =
            M.mapWithKey (\cs pts ->
                pts { ptsData = NonEmpty.map (offsetTimeSeriesChunk offsetVal')
                                             $ ptsData pts
                    , ptsName = (TS.pack . show) cs
                    }
            )
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "offset"
                                       , "("
                                       , (TS.pack . show) offsetVal'
                                       , ","
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'

removePercentileInDirection :: WorldConstraints
                            -> Direction
                            -> Int
                            -> ReturnData
                            -> ReturnData
removePercentileInDirection _w d p pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go p istatdData }
    where
        go p' istatdData' =
            M.mapMaybeWithKey (removePercentileInDirection' p')
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "remove"
                                       , toInfix d
                                       , "Percentile"
                                       , toPostfix d
                                       , "("
                                       , (TS.pack . show) p'
                                       , ","
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'

        removePercentileInDirection' :: Int
                                     -> CounterSpec
                                     -> ParadoxTimeSeries
                                     -> Maybe ParadoxTimeSeries
        removePercentileInDirection' p' cs pts = do
            let pe = getPercentile p' (ptsData pts)

            ptsData <- NonEmpty.nonEmpty $ removePercentileInDirection'' pe (ptsData pts)

            return $ pts { ptsData = ptsData
                         , ptsName = TS.pack . show $ cs
                         }

        removePercentileInDirection'' pV =
            NonEmpty.filter (\x -> directionComparison d (tscAvg x) pV)


removeBelowPercentile :: WorldConstraints
                      -> Int
                      -> ReturnData
                      -> ReturnData
removeBelowPercentile w = removePercentileInDirection w Below

removeAbovePercentile :: WorldConstraints
                      -> Int
                      -> ReturnData
                      -> ReturnData
removeAbovePercentile w = removePercentileInDirection w Above

removeBelowPercentileInc :: WorldConstraints
                         -> Int
                         -> ReturnData
                         -> ReturnData
removeBelowPercentileInc w = removePercentileInDirection w BelowInc

removeAbovePercentileInc :: WorldConstraints
                         -> Int
                         -> ReturnData
                         -> ReturnData
removeAbovePercentileInc w = removePercentileInDirection w AboveInc

removeValueInDirection :: WorldConstraints
                       -> Direction
                       -> Double
                       -> ReturnData
                       -> ReturnData
removeValueInDirection _w d p pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go p istatdData }
    where
        go p' istatdData' =
            M.mapMaybeWithKey (removeValueInDirection' p')
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "remove"
                                       , toInfix d
                                       , "Value"
                                       , toPostfix d
                                       , "("
                                       , (TS.pack . show) p'
                                       , ","
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'

        removeValueInDirection' :: Double
                                -> CounterSpec
                                -> ParadoxTimeSeries
                                -> Maybe ParadoxTimeSeries
        removeValueInDirection' p' cs pts = do
            ptsData <- NonEmpty.nonEmpty $ removeValueInDirection'' p' (ptsData pts)
            return $ pts { ptsData = ptsData
                         , ptsName = TS.pack . show $ cs
                         }

        removeValueInDirection'' pV =
            NonEmpty.filter (\x -> directionComparison d (tscAvg x) pV)


removeBelowValue :: WorldConstraints
                 -> Double
                 -> ReturnData
                 -> ReturnData
removeBelowValue w = removeValueInDirection w Below

removeAboveValue :: WorldConstraints
                 -> Double
                 -> ReturnData
                 -> ReturnData
removeAboveValue w = removeValueInDirection w Above

removeBelowValueInc :: WorldConstraints
                    -> Double
                    -> ReturnData
                    -> ReturnData
removeBelowValueInc w = removeValueInDirection w BelowInc

removeAboveValueInc :: WorldConstraints
                    -> Double
                    -> ReturnData
                    -> ReturnData
removeAboveValueInc w = removeValueInDirection w AboveInc

capValueInDirection :: WorldConstraints
                    -> Direction
                    -> TS.Text
                    -> Double
                    -> ReturnData
                    -> ReturnData
capValueInDirection _w d np p pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go p istatdData }
    where
        go p' istatdData' =
            M.mapMaybeWithKey (capValueInDirection' p')
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ np
                                       , "("
                                       , (TS.pack . show) p'
                                       , ","
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'

        capValueInDirection' :: Double
                             -> CounterSpec
                             -> ParadoxTimeSeries
                             -> Maybe ParadoxTimeSeries
        capValueInDirection' p' cs pts = do
            let ptsData' = capValueInDirection'' p' (ptsData pts)
            return $ pts { ptsData = ptsData'
                         , ptsName = TS.pack . show $ cs
                         }

        capValueInDirection'' pV =
            NonEmpty.map (\x -> if directionComparison d (tscAvg x) pV
                                   then x
                                   else x { tscAvg = pV
                                          , tscMin = pV
                                          , tscMax = pV
                                          , tscSdev = 0
                                          }
                         )

ceiling :: WorldConstraints
        -> Double
        -> ReturnData
        -> ReturnData
ceiling w = capValueInDirection w Above "ceiling"

floor :: WorldConstraints
      -> Double
      -> ReturnData
      -> ReturnData
floor w = capValueInDirection w Below "floor"

medianAbsoluteDeviation :: WorldConstraints
                        -> ReturnData
                        -> ReturnData
medianAbsoluteDeviation _w pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go istatdData }
    where
        go :: TSMap
           -> TSMap
        go istatdData' =
            M.mapMaybeWithKey (medianAbsoluteDeviationImpl (const True) snd)
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "medianAbsoluteDeviation("
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'

medianAbsoluteDeviationFilter :: WorldConstraints
                              -> Int
                              -> ReturnData
                              -> ReturnData
medianAbsoluteDeviationFilter _w thresh pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go thresh istatdData }
    where
        go :: Int
           -> TSMap
           -> TSMap
        go thresh' istatdData' =
            M.mapMaybeWithKey (medianAbsoluteDeviationImpl (\(_,ma) -> tscAvg ma > fromIntegral thresh') fst)
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "medianAbsoluteDeviationFilter("
                                       , (TS.pack . show) thresh'
                                       , ","
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'


medianAbsoluteDeviationImpl :: ((TimeSeriesChunk, TimeSeriesChunk) -> Bool)
                            -> ((TimeSeriesChunk, TimeSeriesChunk) -> TimeSeriesChunk)
                            -> CounterSpec
                            -> ParadoxTimeSeries
                            -> Maybe ParadoxTimeSeries
medianAbsoluteDeviationImpl dec sel cs ts = do
            let med = median'' (NonEmpty.length $ ptsData ts) $ ptsData ts
                newData = NonEmpty.map (\x -> x { tscAvg = abs $ tscAvg x })
                        $ NonEmpty.map (`subTimeSeriesChunk` med)
                        $ ptsData ts
                mad = median'' (NonEmpty.length newData) newData
                finalData = NonEmpty.map (\x -> divTimeSeriesChunk (x { tscAvg = abs $ tscAvg x }) mad)
                          $ NonEmpty.map (`subTimeSeriesChunk` med)
                          $ ptsData ts
                included = map sel $ NonEmpty.filter dec $ NonEmpty.zip (ptsData ts) finalData

            includedData <- NonEmpty.nonEmpty included



            return $ ts { ptsData = includedData
                        , ptsName = TS.pack . show $ cs
                        }

doubleMedianAbsoluteDeviation :: WorldConstraints
                              -> ReturnData
                              -> ReturnData
doubleMedianAbsoluteDeviation _w pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go istatdData }
    where
        go :: TSMap
           -> TSMap
        go istatdData' =
            M.mapMaybeWithKey (doubleMedianAbsoluteDeviationImpl (const True) snd)
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "doubleMedianAbsoluteDeviation("
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'

doubleMedianAbsoluteDeviationFilter :: WorldConstraints
                                    -> Int
                                    -> ReturnData
                                    -> ReturnData
doubleMedianAbsoluteDeviationFilter _w thresh pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go thresh istatdData }
    where
        go :: Int
           -> TSMap
           -> TSMap
        go thresh' istatdData' =
            M.mapMaybeWithKey (doubleMedianAbsoluteDeviationImpl (\(_,ma) -> tscAvg ma > fromIntegral thresh') fst)
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "doubleMedianAbsoluteDeviationFilter("
                                       , (TS.pack . show) thresh'
                                       , ","
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'

doubleMedianAbsoluteDeviationImpl :: ((TimeSeriesChunk, TimeSeriesChunk) -> Bool)
                                  -> ((TimeSeriesChunk, TimeSeriesChunk) -> TimeSeriesChunk)
                                  -> CounterSpec
                                  -> ParadoxTimeSeries
                                  -> Maybe ParadoxTimeSeries
doubleMedianAbsoluteDeviationImpl dec sel cs ts = do
            let med = median'' (NonEmpty.length $ ptsData ts) $ ptsData ts
                newData = NonEmpty.map (`subTimeSeriesChunk` med)
                        $ ptsData ts

                medianSplit [] !l !r = (l,r)
                medianSplit (a:as) !l !r = case tscAvg a `compare` 0 of
                                                LT -> medianSplit as (a:l) r
                                                EQ -> medianSplit as (a:l) (a:r)
                                                GT -> medianSplit as l (a:r)

                sorted = sortTSByAvg newData
                (leftM, rightM) = medianSplit (NonEmpty.toList sorted) [] []

            left' <- NonEmpty.nonEmpty leftM
            right' <- NonEmpty.nonEmpty rightM

            let (left,right) = (NonEmpty.map (\x -> x { tscAvg = abs $ tscAvg x} ) left', NonEmpty.map (\x -> x { tscAvg = abs $ tscAvg x }) right')
                (lmad,rmad) = (median'' (NonEmpty.length left) left, median'' (NonEmpty.length right) right)
                chooseMad x = case tscAvg x `compare` 0 of
                                LT -> lmad
                                EQ -> lmad { tscAvg = 1, tscSum = 1, tscSumsq = 1, tscMin = 1, tscMax = 1 }
                                GT -> rmad

                finalData = NonEmpty.map (\x -> divTimeSeriesChunk (x { tscAvg = abs $ tscAvg x }) (chooseMad x))
                          $ NonEmpty.map (`subTimeSeriesChunk` med)
                          $ ptsData ts
                included = map sel $ NonEmpty.filter dec $ NonEmpty.zip (ptsData ts) finalData

            includedData <- NonEmpty.nonEmpty included



            return $ ts { ptsData = includedData
                        , ptsName = TS.pack . show $ cs
                        }


loess :: WorldConstraints
      -> Double
      -> ReturnData
      -> ReturnData
loess _w d pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go istatdData }
    where
        go :: TSMap
           -> TSMap
        go istatdData' =
            M.mapWithKey impl
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "loess("
                                       , (TS.pack . show) d
                                       , ","
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'
        impl cs ts =
            ts { ptsData = Anomaly.loess (ptsData ts) d
                        , ptsName = TS.pack . show $ cs
                        }

edmMulti :: WorldConstraints
         -> Int
         -> Double
         -> ReturnData
         -> ReturnData
edmMulti _w m d pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go istatdData }
    where
        go :: TSMap
           -> TSMap
        go istatdData' =
            M.mapMaybeWithKey impl
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "edmMulti("
                                       , (TS.pack . show) m
                                       , ","
                                       , (TS.pack . show) d
                                       , ","
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'
        impl cs ts = do
            newTSData <- Anomaly.edmMulti (ptsData ts) m d Anomaly.Linear
            return $ ts { ptsData = newTSData
                        , ptsName = TS.pack . show $ cs
                        }

stl :: WorldConstraints
    -> ReturnData
    -> ReturnData
stl _w pt@ParadoxReturn { timeSeriesMap = istatdData, axisOptions = ao, options = o } =
    pt { timeSeriesMap = go istatdData, axisOptions = ao, options = o <> (IT.MappedOptions $ M.fromList [("y2", IT.DrawOptionNV)])  }
    where
        go :: TSMap
           -> TSMap
        go istatdData' =
            M.fromList $
            concat $
            M.elems $
            M.mapWithKey impl istatdData'
        impl :: CounterSpec
             -> ParadoxTimeSeries
             -> [(CounterSpec, ParadoxTimeSeries)]
        impl cs ts =
            let (season, trend, resid) = Anomaly.stl (ptsData ts) (ptsInterval ts)
                toName pre post = case post of
                    Just post' -> CounterSpec (TS.concat [pre, "(", TS.pack . show $ cs, ")", " ", post']) Nothing
                    Nothing -> CounterSpec (TS.concat [pre, "(", TS.pack . show $ cs, ")"]) Nothing
                (s, t, r) = (toName "stlSeason" (Just "(y2)"), toName "stlTrend" Nothing, toName "stlResidual" (Just "(y2)"))
                (s', t', r') = (Just $ IT.MappedOptions $ M.fromList [("axis", IT.DrawOptionS "y2")], Nothing, Just $ IT.MappedOptions $ M.fromList [("axis", IT.DrawOptionS "y2")])
                pairs =  zip3 [season, trend, resid] [s, t, r] [s', t', r']
            in  map (\(res, cs', mOpt) -> (,) cs' ts { ptsData = res
                                                     , ptsName = TS.pack . show $ cs'
                                                     , ptsDrawOptions = case mOpt of
                                                            Just o' -> ptsDrawOptions ts <> o'
                                                            Nothing -> ptsDrawOptions ts
                                                     }
                    ) pairs

stlSeason :: WorldConstraints
          -> ReturnData
          -> ReturnData
stlSeason _w pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go istatdData }
    where
        go :: TSMap
           -> TSMap
        go istatdData' =
            M.mapWithKey impl
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "stlSeason("
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'
        impl cs ts =
            ts { ptsData = Anomaly.stlSeason (ptsData ts) (ptsInterval ts)
                        , ptsName = TS.pack . show $ cs
                        }

stlTrend :: WorldConstraints
         -> ReturnData
         -> ReturnData
stlTrend _w pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go istatdData }
    where
        go :: TSMap
           -> TSMap
        go istatdData' =
            M.mapWithKey impl
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "stlTrend("
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'
        impl cs ts =
            ts { ptsData = Anomaly.stlTrend (ptsData ts) (ptsInterval ts)
                        , ptsName = TS.pack . show $ cs
                        }

stlResidual :: WorldConstraints
            -> ReturnData
            -> ReturnData
stlResidual _w pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go istatdData }
    where
        go :: TSMap
           -> TSMap
        go istatdData' =
            M.mapWithKey impl
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "stlResidual("
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'
        impl cs ts =
            ts { ptsData = Anomaly.stlResidual (ptsData ts) (ptsInterval ts)
                        , ptsName = TS.pack . show $ cs
                        }

medianFilter :: WorldConstraints
             -> Int
             -> ReturnData
             -> ReturnData
medianFilter _w lag pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go lag istatdData }
    where
        go :: Int
           -> TSMap
           -> TSMap
        go lag' istatdData' =
            M.mapMaybeWithKey (medianFilterTS lag')
            $ M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "medianFilter("
                                       , (TS.pack . show) lag'
                                       , ","
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            ) istatdData'

        medianFilterTS :: Int
                       -> CounterSpec
                       -> ParadoxTimeSeries
                       -> Maybe ParadoxTimeSeries
        medianFilterTS lag' cs ts = do
            a <- medianFilterL lag' $ ptsData ts

            let a' = NonEmpty.toList a
                (pL, pR) = computePadding lag'
                pad p = replicate p def
                (hL, hR) = (pad pL, pad pR)
                r = foldr (:) a' hL
                r' = reverse $ foldr (:) (reverse r) hR

            ptsData <- NonEmpty.nonEmpty r'

            return $ ts { ptsData = ptsData
                        , ptsName = TS.pack . show $ cs
                        }


        medianFilterL :: Int
                      -> NonEmpty TimeSeriesChunk
                      -> Maybe (NonEmpty TimeSeriesChunk)
        medianFilterL lag' ats = do
            let tailTS = mapMaybe NonEmpty.nonEmpty $ tails lag' (NonEmpty.toList ats)
            NonEmpty.nonEmpty $ map (median'' lag') tailTS

computeIdx :: Integral a
           => a
           -> a
computeIdx lag' =
    let dbl :: Double
        dbl = (fromIntegral lag' + 1)
    in Prelude.ceiling (dbl / 2) - 1

computePadding :: Integral a
               => a
               -> (a, a)
computePadding lag' =
    let idx = computeIdx lag'
    in (idx, (lag' - 1) - idx)



median'' :: Int
            -> NonEmpty TimeSeriesChunk
            -> TimeSeriesChunk
median'' lag' tts =
    let sorted = sortTSByAvg tts
        medianV = selectMedian lag' sorted
        computedIdx = computeIdx lag'
        TimeSeriesChunk { tscTime = baseTime } = tts NonEmpty.!! computedIdx
    in medianV { tscTime = baseTime }

selectMedian :: Int
                -> NonEmpty TimeSeriesChunk
                -> TimeSeriesChunk
selectMedian lag' sorted =
    case toMedian lag' of
        Single -> selectSingleMedian (computeIdx lag') sorted
        Multiple ->
            let (divI, modI) = (lag' + 1) `divMod` 2
            in selectMultipleMedian divI (divI + modI) sorted

selectSingleMedian :: Int
                    -> NonEmpty TimeSeriesChunk
                    -> TimeSeriesChunk
selectSingleMedian medianIdx sorted = sorted NonEmpty.!! medianIdx

selectMultipleMedian :: Int
                        -> Int
                        -> NonEmpty TimeSeriesChunk
                        -> TimeSeriesChunk
selectMultipleMedian medianIdx medianIdx2 sorted =
    let f = sorted NonEmpty.!! medianIdx
        f2 = sorted NonEmpty.!! medianIdx2
    in divTimeSeriesChunkScalar 2 $ foldr1 addTimeSeriesChunk [f,f2]

data Median = Single | Multiple
            deriving (Show)

toMedian :: Integral a
         => a
         -> Median
toMedian lag = toMedian' $ (lag + 1) `mod` 2

toMedian' :: Integral a
          => a
          -> Median
toMedian' 0 = Single
toMedian' 1 = Multiple
toMedian' x = toMedian' x


movingAverage :: WorldConstraints
              -> Int
              -> ReturnData
              -> ReturnData
movingAverage _w lag pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = go lag istatdData }
    where

        go :: Int
           -> TSMap
           -> TSMap
        go lag' =
            M.mapMaybeWithKey (mvAvgTS lag')
            . M.mapKeys
            (\x ->
                CounterSpec (TS.concat [ "movingAverage("
                                       , (TS.pack . show) lag'
                                       , ","
                                       , (TS.pack . show) x
                                       , ")"
                                       ]
                            )
                            Nothing
            )

        mvAvgTS :: Int
                -> CounterSpec
                -> ParadoxTimeSeries
                -> Maybe ParadoxTimeSeries
        mvAvgTS lag' cs ts = do
            a <- mvAvgL lag' $ ptsData ts
            let a' = NonEmpty.toList a
                h = replicate ( NonEmpty.length (ptsData ts) - length a') def
                r = foldr (:) a' h
            ptsData <- NonEmpty.nonEmpty r

            return $ ts { ptsData = ptsData
                        , ptsName = TS.pack . show $ cs
                        }

        mvAvgL :: Int
               -> NonEmpty TimeSeriesChunk
               -> Maybe (NonEmpty TimeSeriesChunk)
        mvAvgL lag' ats = do
            let tailTS = mapMaybe NonEmpty.nonEmpty $ tails lag' (NonEmpty.toList ats)
            NonEmpty.nonEmpty $ map mvAvg'' tailTS

        mvAvg'' :: NonEmpty TimeSeriesChunk
                -> TimeSeriesChunk
        mvAvg'' tts = divTimeSeriesChunkScalar (fromIntegral $ NonEmpty.length tts) $ foldr1 (flip addTimeSeriesChunk) tts

scale :: WorldConstraints
      -> Double
      -> ReturnData
      -> ReturnData
scale _w scale' pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = M.mapWithKey (scale'' scale')
        $ M.mapKeys
            (\x ->
                CounterSpec
                    (TS.concat [ "scale("
                               , (TS.pack . show) scale'
                               ,","
                               , (TS.pack . show) x
                               , ")"
                               ]
                    )
                    Nothing
            )
            istatdData }
    where
        scale'' x cs pts =
            pts { ptsData = NonEmpty.map (mulTimeSeriesChunkScalar x) $ ptsData pts
                , ptsName = (TS.pack . show) cs
                }

scalarDivide :: WorldConstraints
             -> Double
             -> ReturnData
             -> ReturnData
scalarDivide _w divide' pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = M.mapWithKey (divide'' divide')
        $ M.mapKeys
            (\x ->
                CounterSpec
                    (TS.concat [ "scalarDivide("
                               , (TS.pack . show) divide'
                               ,","
                               , (TS.pack . show) x
                               , ")"
                               ]
                    )
                    Nothing
            )
            istatdData }
    where
        divide'' x cs pts =
            pts { ptsData = NonEmpty.map (divTimeSeriesChunkScalar x) $ ptsData pts
                , ptsName = (TS.pack . show) cs
                }

scaleByInterval :: WorldConstraints
                -> ReturnData
                -> ReturnData
scaleByInterval _w pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = M.mapWithKey scale'
        $ M.mapKeys
            (\x ->
                CounterSpec
                    (TS.concat [ "scaleByInterval("
                               , (TS.pack . show) x
                               , ")"
                               ]
                    )
                    Nothing
            )
            istatdData }
    where
        scale' cs pts =
            pts { ptsData =
                    let op = mulTimeSeriesChunkScalar (fromIntegral . ptsInterval $ pts)
                    in NonEmpty.map op (ptsData pts)
                , ptsName = (TS.pack . show) cs
                }

divideByInterval :: WorldConstraints
                 -> ReturnData
                 -> ReturnData
divideByInterval _w pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = M.mapWithKey scale'
        $ M.mapKeys
            (\x ->
                CounterSpec
                    (TS.concat [ "divideByInterval("
                               , (TS.pack . show) x
                               , ")"
                               ]
                    )
                    Nothing
            )
            istatdData }
    where
        scale' cs pts =
            pts { ptsData =
                    let op = divTimeSeriesChunkScalar (fromIntegral . ptsInterval $ pts)
                    in NonEmpty.map op $ ptsData pts
                , ptsName = (TS.pack . show) cs
                }

averageTimeSeries :: WorldConstraints
                  -> ReturnData
                  -> ReturnData
averageTimeSeries _w pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = M.singleton (CounterSpec newName Nothing) avgd }
    where
        (seriesNames, _) = (map (TS.pack . show) $ M.keys istatdData, M.elems istatdData)
        newName = TS.concat [ "avg("
                            , TS.intercalate "," seriesNames
                            , ")"
                            ]
        total = fromIntegral $ M.size istatdData

        avgd = divTSScalar newName total $ foldr1 sum' istatdData
        sum' = addTS newName


sumify :: WorldConstraints
       -> ReturnData
       -> ReturnData
sumify _w pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = M.singleton (CounterSpec newName Nothing) summed }
    where
        (seriesNames, _) = (map (TS.pack . show) $ M.keys istatdData, M.elems istatdData)
        newName = TS.concat [ "sum("
                            , TS.intercalate "," seriesNames
                            , ")"
                            ]

        summed = foldr1 sum' istatdData
        sum' = addTS newName

graphField ::WorldConstraints
           -> (TS.Text, TimeSeriesChunk -> Double)
           -> ReturnData
           -> ReturnData
graphField _w (fname, f) pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = M.mapWithKey (\(CounterSpec k _) v -> graphField' k v)
        $ M.mapKeys
            (\x ->
                CounterSpec
                    (TS.concat [ "graph"
                               , fname
                               , "("
                               , (TS.pack . show) x
                               , ")"
                               ]
                    )
                    Nothing
            )
            istatdData }
    where
        graphField' :: TS.Text
                    -> ParadoxTimeSeries
                    -> ParadoxTimeSeries
        graphField' newName v =
            let i = mkField v
            in i { ptsName = newName }

        mkField :: ParadoxTimeSeries
                -> ParadoxTimeSeries
        mkField pts =
            let ptsData = IT.ptsData pts
            in pts { IT.ptsData = NonEmpty.map modify ptsData}

        modify :: TimeSeriesChunk
               -> TimeSeriesChunk
        modify e = e { IT.tscAvg = f e
                     , IT.tscMax = f e
                     , IT.tscMin = f e
                     , IT.tscSdev = 0
                     }

maxAtSamples :: WorldConstraints
              -> (TS.Text, TimeSeriesChunk -> Double)
              -> ReturnData
              -> ReturnData
maxAtSamples _w (fname, f) pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = M.singleton (CounterSpec newName Nothing) maxed }
    where
        (seriesNames, _) = (map (TS.pack . show) $ M.keys istatdData, M.elems istatdData)
        newName = TS.concat [ "maxAtSamples"
                            , fname
                            , "("
                            , TS.intercalate "," seriesNames
                            , ")"
                            ]

        maxed = foldr1 max' istatdData
        max' = cmpGTTS f newName


divideTimeSeries :: WorldConstraints
                 -> ReturnData
                 -> ReturnData
                 -> ReturnData
divideTimeSeries _w
                 ParadoxReturn { timeSeriesMap = name }
                 pt@ParadoxReturn { timeSeriesMap = istatdData } =
    case divided of
        Just elems -> pt { timeSeriesMap = M.fromList [( CounterSpec newName Nothing, elems)] }
        Nothing -> pt
    where
        newName = TS.concat [ "divide("
                            , names
                            , ")"
                            ]
        tShow = TS.pack . show
        names = TS.intercalate
                    ","
                    $ tShow first : map tShow (M.keys istatdData)

        (first, divFrom) = M.elemAt 0 name
        divided = do
            summedData <- summed
            return $ div' divFrom summedData
        summed
            | (not . M.null) istatdData = Just $ foldr1 sum' istatdData
            | otherwise = Nothing
        sum' = addTS newName
        div' = divTS newName

subtractify :: WorldConstraints
            -> ReturnData
            -> ReturnData
            -> ReturnData
subtractify _w
            ParadoxReturn { timeSeriesMap = name}
            pt@ParadoxReturn { timeSeriesMap = istatdData } =
    case subtracted of
        Just elems -> pt { timeSeriesMap = M.fromList [( CounterSpec newName Nothing, elems)] }
        Nothing -> pt
    where
        newName = TS.concat [ "subtract("
                            , names
                            , ")"
                            ]
        tShow = TS.pack . show
        names = TS.intercalate
                    ","
                    $ tShow first : map tShow (M.keys istatdData)
        (first, subFrom) = M.elemAt 0 name
        subtracted = do
            summedData <- summed
            return $ sub' subFrom summedData
        summed
            | (not . M.null) istatdData = Just $ foldr1 sum' istatdData
            | otherwise = Nothing
        sum' = addTS newName
        sub' = subTS newName

integrify :: WorldConstraints
          -> ReturnData
          -> ReturnData
integrify _w pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = M.mapWithKey (\(CounterSpec k _) v -> integrate' k v)
        $ M.mapKeys
            (\x ->
                CounterSpec
                    (TS.concat [ "integrate("
                               , (TS.pack . show) x
                               , ")"
                               ]
                    )
                    Nothing
            )
            istatdData }
    where
        integrate' :: TS.Text
                   -> ParadoxTimeSeries
                   -> ParadoxTimeSeries
        integrate' newName v =
            let i = integrate v
            in i { ptsName = newName }

        integrate :: ParadoxTimeSeries
                  -> ParadoxTimeSeries
        integrate pts =
            let ptsData = IT.ptsData pts
                initBucket = zeroTSC $ NonEmpty.head ptsData
            in pts { IT.ptsData = snd . mapAccumL modify initBucket $ ptsData}

        modify :: TimeSeriesChunk
               -> TimeSeriesChunk
               -> (TimeSeriesChunk , TimeSeriesChunk)
        modify acc a = let n = a { IT.tscAvg = IT.tscAvg a + IT.tscAvg acc
                                 , IT.tscMax = IT.tscMax a + IT.tscMax acc
                                 , IT.tscMin = IT.tscMin a + IT.tscMin acc
                                 , IT.tscSdev = IT.tscSdev a + IT.tscSdev acc
                                 , IT.tscCount = IT.tscCount a + IT.tscCount acc
                                 , IT.tscSum = IT.tscSum a + IT.tscSum acc
                                 , IT.tscSumsq = IT.tscSumsq a + IT.tscSumsq acc
                                 }
                       in (n, n)

derivify :: WorldConstraints
         -> ReturnData
         -> ReturnData
derivify _w pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = M.mapWithKey (\(CounterSpec k _) v -> derive' k v)
        $ M.mapKeys
            (\x ->
                CounterSpec
                    (TS.concat [ "derive("
                               , (TS.pack . show) x
                               , ")"
                               ]
                    )
                    Nothing
            )
            istatdData }
    where
        fixScaling :: ParadoxTimeSeries
                   -> ParadoxTimeSeries
        fixScaling pts =
            pts { ptsData =
                 let op = divTimeSeriesChunkScalar (fromIntegral . IT.ptsInterval $ pts)
                 in  NonEmpty.map op $ ptsData pts
              }

        derive' :: TS.Text
                -> ParadoxTimeSeries
                -> ParadoxTimeSeries
        derive' newName v =
            let i = fixScaling $ derive v
            in i { ptsName = newName }

        derive :: ParadoxTimeSeries
               -> ParadoxTimeSeries
        derive pts =
            let ptsData = IT.ptsData pts
                initBucket = NonEmpty.head ptsData
            in pts { IT.ptsData = snd . mapAccumL modify initBucket $ ptsData}

        modify :: TimeSeriesChunk
               -> TimeSeriesChunk
               -> (TimeSeriesChunk , TimeSeriesChunk)
        modify acc a = ( a
                       , a { IT.tscAvg = IT.tscAvg a - IT.tscAvg acc
                           , IT.tscMax = IT.tscMax a - IT.tscMax acc
                           , IT.tscMin = IT.tscMin a - IT.tscMin acc
                           , IT.tscSdev = IT.tscSdev a - IT.tscSdev acc
                           , IT.tscCount = IT.tscCount a - IT.tscCount acc
                           , IT.tscSum = IT.tscSum a - IT.tscSum acc
                           , IT.tscSumsq = IT.tscSumsq a - IT.tscSumsq acc
                           }
                       )

logP :: WorldConstraints
     -> Int
     -> Double
     -> Int
     -> ReturnData
     -> ReturnData
logP _w base valueOffset bias pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = M.mapWithKey (\(CounterSpec k _) v -> log' k v)
        $ M.mapKeys
            (\x ->
                CounterSpec
                    (TS.concat [ "log("
                               , (TS.pack . show) base
                               , ", "
                               , (TS.pack . show) valueOffset
                               , ", "
                               , (TS.pack . show) bias
                               , ", "
                               , (TS.pack . show) x
                               , ")"
                               ]
                    )
                    Nothing
            )
            istatdData }
    where
        log' :: TS.Text
             -> ParadoxTimeSeries
             -> ParadoxTimeSeries
        log' newName v =
            let i = log'' v
            in i { ptsName = newName }

        log'' :: ParadoxTimeSeries
              -> ParadoxTimeSeries
        log'' pts =
            let ptsData = IT.ptsData pts
            in pts { IT.ptsData = fmap modify ptsData}

        modify :: TimeSeriesChunk
               -> TimeSeriesChunk
        modify a =
          let
            logOp :: Double -> Double
            logOp v = logBase (fromIntegral base) (v + valueOffset) + fromIntegral bias
          in
            a { IT.tscAvg = logOp (IT.tscAvg a)
              , IT.tscMax = logOp (IT.tscMax a)
              , IT.tscMin = logOp (IT.tscMin a)
              , IT.tscSdev = logOp (IT.tscSdev a)
              , IT.tscCount = IT.tscCount a
              , IT.tscSum = logOp (IT.tscSum a)
              , IT.tscSumsq = logOp (IT.tscSumsq a)
              }

absTimeSeries :: WorldConstraints
              -> ReturnData
              -> ReturnData
absTimeSeries _w pt@ParadoxReturn { timeSeriesMap = istatdData } =
    pt { timeSeriesMap = M.mapWithKey (\(CounterSpec k _) v -> absPTS k v)
        $ M.mapKeys
            (\x ->
                CounterSpec
                    (TS.concat [ "abs("
                               , (TS.pack . show) x
                               , ")"
                               ]
                    )
                    Nothing
            )
            istatdData }
    where
        absPTS :: TS.Text
            -> ParadoxTimeSeries
            -> ParadoxTimeSeries
        absPTS newName v =
            let i = abs' v
            in i { ptsName = newName }

        abs' :: ParadoxTimeSeries
             -> ParadoxTimeSeries
        abs' pts =
            let ptsData = IT.ptsData pts
            in pts { IT.ptsData = NonEmpty.map modify ptsData}

        modify :: TimeSeriesChunk
               -> TimeSeriesChunk
        modify a = a { IT.tscAvg = abs (IT.tscAvg a)
                     , IT.tscMax = abs (IT.tscMax a)
                     , IT.tscMin = abs (IT.tscMin a)
                     }
