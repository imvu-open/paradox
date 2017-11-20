{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Paradox.Functions.Filter (
  exclude
, include
, keepSeriesWithinDeviation
, keepSeriesWithinDeviationInc
, keepSeriesAboveDeviation
, keepSeriesAboveDeviationInc
, keepSeriesAbove
, keepSeriesAboveInc
, keepSeriesBelow
, keepSeriesBelowInc
, keepSeriesAllAbove
, keepSeriesAllAboveInc
, keepSeriesAllBelow
, keepSeriesAllBelowInc
) where

import Prelude hiding                           ( foldl1
                                                , any
                                                , all
                                                )

import Language.Paradox.Eval.Types
import Text.Regex.PCRE

import Data.Foldable                            ( any
                                                , all
                                                )
import Data.List.NonEmpty                       (NonEmpty)
import Paradox.Istatd.Types                     ( TimeSeriesChunk(..)
                                                , ParadoxTimeSeries(..)
                                                , CounterSpec(..)
                                                , ReturnData
                                                , ParadoxReturn(..)
                                                )
import Paradox.Functions.Util                   ( Direction(..)
                                                , directionComparison
                                                )
import Paradox.Functions.Math.TimeSeriesChunk   (getSampleDeviationTS)

import qualified Data.Map.Strict                as M
import qualified Data.Text                      as TS
import qualified Data.Text.Encoding             as TSE

exclude :: WorldConstraints
        -> TS.Text
        -> ReturnData
        -> ReturnData
exclude _w r pt@ParadoxReturn { timeSeriesMap = istatdData } = pt { timeSeriesMap = M.filterWithKey (f r) istatdData }
    where
        f regex' k _ = not $ ((TSE.encodeUtf8 $ csName k) =~) (TSE.encodeUtf8 regex')

include :: WorldConstraints
        -> TS.Text
        -> ReturnData
        -> ReturnData
include _w r pt@ParadoxReturn { timeSeriesMap = istatdData } = pt { timeSeriesMap = M.filterWithKey (f r) istatdData }
    where
        f regex' k _ = ((TSE.encodeUtf8 $ csName k) =~) (TSE.encodeUtf8 regex')

keepSeriesDeviationInDirection :: WorldConstraints
                               -> Direction
                               -> Int
                               -> ReturnData
                               -> ReturnData
keepSeriesDeviationInDirection _w d n pt@ParadoxReturn { timeSeriesMap = istatdData } = pt { timeSeriesMap = go n istatdData }
    where
        go n' =
            M.filter
            (filterDeviationInDirection' n')

        filterDeviationInDirection' :: Int
                                    -> ParadoxTimeSeries
                                    -> Bool
        filterDeviationInDirection' n' pts = let (sdev, mean) = getSampleDeviationTS (ptsData pts)
                                                 dist = sdev * fromIntegral n'
                                                 pointsFn = anyOrAll d
                                           in pointsFn (filterDeviationInDirection'' dist mean) (ptsData pts)

        filterDeviationInDirection'' :: Double
                                     -> Double
                                     -> TimeSeriesChunk
                                     -> Bool
        filterDeviationInDirection'' dist mean tsc = let myDist = abs $ tscAvg tsc - mean
                                                     in directionComparison d dist myDist

        anyOrAll :: Direction
                 ->  (a -> Bool)
                 -> NonEmpty a
                 -> Bool
        anyOrAll = \case
            Above -> any
            AboveInc -> any
            Below -> all
            BelowInc -> all


keepSeriesWithinDeviation :: WorldConstraints
                          -> Int
                          -> ReturnData
                          -> ReturnData
keepSeriesWithinDeviation w = keepSeriesDeviationInDirection w Below

keepSeriesAboveDeviation :: WorldConstraints
                         -> Int
                         -> ReturnData
                         -> ReturnData
keepSeriesAboveDeviation w = keepSeriesDeviationInDirection w Above

keepSeriesWithinDeviationInc :: WorldConstraints
                             -> Int
                             -> ReturnData
                             -> ReturnData
keepSeriesWithinDeviationInc w = keepSeriesDeviationInDirection w BelowInc

keepSeriesAboveDeviationInc :: WorldConstraints
                            -> Int
                            -> ReturnData
                            -> ReturnData
keepSeriesAboveDeviationInc w = keepSeriesDeviationInDirection w AboveInc

data AnyOrAll = Any | All

keepSeries :: WorldConstraints
           -> Direction
           -> AnyOrAll
           -> Double
           -> ReturnData
           -> ReturnData
keepSeries _w d a p pt@ParadoxReturn { timeSeriesMap = istatdData } = pt { timeSeriesMap = go p istatdData }
    where
        go n' =
            M.filter
            (filterInDirection' n')

        filterInDirection' :: Double
                           -> ParadoxTimeSeries
                           -> Bool
        filterInDirection' n' pts = let pointsFn = anyOrAll a
                                    in pointsFn (filterInDirection'' n') (ptsData pts)

        filterInDirection'' :: Double
                            -> TimeSeriesChunk
                            -> Bool
        filterInDirection'' thresh tsc = let myVal = tscAvg tsc
                                         in directionComparison d thresh myVal

        anyOrAll :: AnyOrAll
                 -> (a -> Bool)
                 -> NonEmpty a
                 -> Bool
        anyOrAll = \case
            Any -> any
            All -> all

keepSeriesBelow :: WorldConstraints
                -> Double
                -> ReturnData
                -> ReturnData
keepSeriesBelow w = keepSeries w Below Any

keepSeriesAbove :: WorldConstraints
                -> Double
                -> ReturnData
                -> ReturnData
keepSeriesAbove w = keepSeries w Above Any

keepSeriesBelowInc :: WorldConstraints
                   -> Double
                   -> ReturnData
                   -> ReturnData
keepSeriesBelowInc w = keepSeries w BelowInc Any

keepSeriesAboveInc :: WorldConstraints
                   -> Double
                   -> ReturnData
                   -> ReturnData
keepSeriesAboveInc w = keepSeries w AboveInc Any

keepSeriesAllBelow :: WorldConstraints
                   -> Double
                   -> ReturnData
                   -> ReturnData
keepSeriesAllBelow w = keepSeries w Below All

keepSeriesAllAbove :: WorldConstraints
                   -> Double
                   -> ReturnData
                   -> ReturnData
keepSeriesAllAbove w = keepSeries w Above All

keepSeriesAllBelowInc :: WorldConstraints
                      -> Double
                      -> ReturnData
                      -> ReturnData
keepSeriesAllBelowInc w = keepSeries w BelowInc All

keepSeriesAllAboveInc :: WorldConstraints
                      -> Double
                      -> ReturnData
                      -> ReturnData
keepSeriesAllAboveInc w = keepSeries w AboveInc All
