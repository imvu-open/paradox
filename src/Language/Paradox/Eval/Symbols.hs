{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Paradox.Eval.Symbols where

import Language.Paradox.Eval.Types
import Language.Haskell.Extract                 (functionExtractor)

import Paradox.Functions.Util                   (Summarizer(..))

import qualified Data.Text                      as TS
import qualified Data.Map.Strict                as M

allSymbols :: M.Map TS.Text TypE
allSymbols = M.fromList allSymbols'

allSymbols' :: [(TS.Text, TypE)]
allSymbols' = $(functionExtractor "^symbol[A-Z]")


symbolSum :: TypE
symbolSum = TypE (TT :~> TT) Sum

symbolMaxMaxAtEachSample :: TypE
symbolMaxMaxAtEachSample = TypE (TT :~> TT) (Apply MaxAtEachSample GetMax)

symbolMaxAvgAtEachSample :: TypE
symbolMaxAvgAtEachSample = TypE (TT :~> TT) (Apply MaxAtEachSample GetAvg)

symbolMaxMinAtEachSample :: TypE
symbolMaxMinAtEachSample = TypE (TT :~> TT) (Apply MaxAtEachSample GetMin)

symbolMaxValueAtEachSample :: TypE
symbolMaxValueAtEachSample = TypE ( TTag TS (TTSC :~> TD) :~> TT :~> TT) MaxAtEachSample

symbolGraphCount :: TypE
symbolGraphCount = TypE (TT :~> TT) (Apply GraphField GetCount)

symbolGraphMax :: TypE
symbolGraphMax = TypE (TT :~> TT) (Apply GraphField GetMax)

symbolGraphMin :: TypE
symbolGraphMin = TypE (TT :~> TT) (Apply GraphField GetMin)

symbolGraphAvg :: TypE
symbolGraphAvg = TypE (TT :~> TT) (Apply GraphField GetAvg)

symbolGraphField :: TypE
symbolGraphField = TypE (TTag TS (TTSC :~> TD) :~> TT :~> TT) GraphField

symbolSummarizeAvg :: TypE
symbolSummarizeAvg = TypE (TI :~> TT :~> TT) (Summarize AvgSummarizer)

symbolSummarizeSum :: TypE
symbolSummarizeSum = TypE (TI :~> TT :~> TT) (Summarize SumSummarizer)

symbolAvg :: TypE
symbolAvg = TypE (TT :~> TT) Avg

symbolAbs :: TypE
symbolAbs = TypE (TT :~> TT) Abs

symbolAbsoluteValue :: TypE
symbolAbsoluteValue = TypE (TT :~> TT) Abs

symbolMovingAverage :: TypE
symbolMovingAverage = TypE (TI :~> TT :~> TT) MovingAverage

symbolMedianFilter :: TypE
symbolMedianFilter = TypE (TI :~> TT :~> TT) MedianFilter

symbolMedianAbsoluteDeviation :: TypE
symbolMedianAbsoluteDeviation = TypE (TT :~> TT) MedianAbsoluteDeviation

symbolMedianAbsoluteDeviationFilter :: TypE
symbolMedianAbsoluteDeviationFilter = TypE (TI :~> TT :~> TT) MedianAbsoluteDeviationFilter

symbolDoubleMedianAbsoluteDeviation :: TypE
symbolDoubleMedianAbsoluteDeviation = TypE (TT :~> TT) DoubleMedianAbsoluteDeviation

symbolDoubleMedianAbsoluteDeviationFilter :: TypE
symbolDoubleMedianAbsoluteDeviationFilter = TypE (TI :~> TT :~> TT) DoubleMedianAbsoluteDeviationFilter

symbolLoess :: TypE
symbolLoess = TypE (TD :~> TT :~> TT) Loess

symbolEDMMulti :: TypE
symbolEDMMulti = TypE (TI :~> TD :~> TT :~> TT) EDMMulti

symbolStl :: TypE
symbolStl = TypE (TT :~> TT) Stl

symbolStlSeason :: TypE
symbolStlSeason = TypE (TT :~> TT) StlSeason

symbolStlTrend :: TypE
symbolStlTrend = TypE (TT :~> TT) StlTrend

symbolStlResidual :: TypE
symbolStlResidual = TypE (TT :~> TT) StlResidual

symbolOffset :: TypE
symbolOffset = TypE (TD :~> TT :~> TT) Offset

symbolKeepSeriesAboveDeviation :: TypE
symbolKeepSeriesAboveDeviation = TypE (TI :~> TT :~> TT) KeepSeriesAboveDeviation

symbolKeepSeriesAboveDeviationInc :: TypE
symbolKeepSeriesAboveDeviationInc = TypE (TI :~> TT :~> TT) KeepSeriesAboveDeviationInc

symbolKeepSeriesWithinDeviation :: TypE
symbolKeepSeriesWithinDeviation = TypE (TI :~> TT :~> TT) KeepSeriesWithinDeviation

symbolKeepSeriesWithinDeviationInc :: TypE
symbolKeepSeriesWithinDeviationInc = TypE (TI :~> TT :~> TT) KeepSeriesWithinDeviationInc

symbolRemoveAbovePercentile :: TypE
symbolRemoveAbovePercentile = TypE (TI :~> TT :~> TT) RemoveAbovePercentile

symbolRemoveAbovePercentileInc :: TypE
symbolRemoveAbovePercentileInc = TypE (TI :~> TT :~> TT) RemoveAbovePercentileInc

symbolRemoveBelowPercentile :: TypE
symbolRemoveBelowPercentile = TypE (TI :~> TT :~> TT) RemoveBelowPercentile

symbolRemoveBelowPercentileInc :: TypE
symbolRemoveBelowPercentileInc = TypE (TI :~> TT :~> TT) RemoveBelowPercentileInc

symbolRemoveAboveValue :: TypE
symbolRemoveAboveValue = TypE (TD :~> TT :~> TT) RemoveAboveValue

symbolRemoveAboveValueInc :: TypE
symbolRemoveAboveValueInc = TypE (TD :~> TT :~> TT) RemoveAboveValueInc

symbolRemoveBelowValue :: TypE
symbolRemoveBelowValue = TypE (TD :~> TT :~> TT) RemoveBelowValue

symbolRemoveBelowValueInc :: TypE
symbolRemoveBelowValueInc = TypE (TD :~> TT :~> TT) RemoveBelowValueInc

symbolCeiling :: TypE
symbolCeiling = TypE (TD :~> TT :~> TT) Ceiling

symbolFloor :: TypE
symbolFloor = TypE (TD :~> TT :~> TT) Floor

symbolKeepSeriesAbove :: TypE
symbolKeepSeriesAbove = TypE (TD :~> TT :~> TT) KeepSeriesAbove

symbolKeepSeriesAboveInc :: TypE
symbolKeepSeriesAboveInc = TypE (TD :~> TT :~> TT) KeepSeriesAboveInc

symbolKeepSeriesBelow :: TypE
symbolKeepSeriesBelow = TypE (TD :~> TT :~> TT) KeepSeriesBelow

symbolKeepSeriesBelowInc :: TypE
symbolKeepSeriesBelowInc = TypE (TD :~> TT :~> TT) KeepSeriesBelowInc

symbolKeepSeriesAllAbove :: TypE
symbolKeepSeriesAllAbove = TypE (TD :~> TT :~> TT) KeepSeriesAllAbove

symbolKeepSeriesAllAboveInc :: TypE
symbolKeepSeriesAllAboveInc = TypE (TD :~> TT :~> TT) KeepSeriesAllAboveInc

symbolKeepSeriesAllBelow :: TypE
symbolKeepSeriesAllBelow = TypE (TD :~> TT :~> TT) KeepSeriesAllBelow

symbolKeepSeriesAllBelowInc :: TypE
symbolKeepSeriesAllBelowInc = TypE (TD :~> TT :~> TT) KeepSeriesAllBelowInc

symbolAlias :: TypE
symbolAlias = TypE (TS :~> TT :~> TT) Alias

symbolSmartAlias :: TypE
symbolSmartAlias = TypE (TS :~> TI :~> TT :~> TT) SmartAlias

symbolSmartAliasA :: TypE
symbolSmartAliasA = TypE (TS :~> TIA :~> TT :~> TT) SmartAliasA

symbolExclude :: TypE
symbolExclude = TypE (TS :~> TT :~> TT) Exclude

symbolInclude :: TypE
symbolInclude = TypE (TS :~> TT :~> TT) Include

symbolScale :: TypE
symbolScale = TypE (TD :~> TT :~> TT) Scale

symbolScalarDivide :: TypE
symbolScalarDivide = TypE (TD :~> TT :~> TT) ScalarDivide

symbolScaleByInterval :: TypE
symbolScaleByInterval = TypE (TT :~> TT) ScaleByInterval

symbolDivideByInterval :: TypE
symbolDivideByInterval = TypE (TT :~> TT) DivideByInterval

symbolIntegrate :: TypE
symbolIntegrate = TypE (TT :~> TT) Integrate

symbolDerive :: TypE
symbolDerive = TypE (TT :~> TT) Derive

symbolLog :: TypE
symbolLog = TypE (TI :~> TD :~> TI :~> TT :~> TT) Log

symbolSub :: TypE
symbolSub = TypE (TT :~> TT :~> TT) Sub

symbolSubtract :: TypE
symbolSubtract = TypE (TT :~> TT :~> TT) Sub

symbolDivide :: TypE
symbolDivide = TypE (TT :~> TT :~> TT) Divide

symbolCons :: TypE
symbolCons = TypE (TT :~> TT :~> TT) Cons

symbolConstantLine :: TypE
symbolConstantLine = TypE (TD :~> TT) ConstantLine

symbolGraphOptions :: TypE
symbolGraphOptions = TypE (TS :~> TT :~> TT) GraphOpts


symbolGraphOptionI :: TypE
symbolGraphOptionI = TypE (TS :~> TI :~> TT :~> TT) GraphOptI

symbolGraphOptionD :: TypE
symbolGraphOptionD = TypE (TS :~> TD :~> TT :~> TT) GraphOptD

symbolGraphOptionS :: TypE
symbolGraphOptionS = TypE (TS :~> TS :~> TT :~> TT) GraphOptS

symbolGraphOptionB :: TypE
symbolGraphOptionB = TypE (TS :~> TB :~> TT :~> TT) GraphOptB

symbolGraphOptionIA :: TypE
symbolGraphOptionIA = TypE (TS :~> TIA :~> TT :~> TT) GraphOptIA

symbolGraphOptionDA :: TypE
symbolGraphOptionDA = TypE (TS :~> TDA :~> TT :~> TT) GraphOptDA

symbolGraphOptionSA :: TypE
symbolGraphOptionSA = TypE (TS :~> TSA :~> TT :~> TT) GraphOptSA

symbolGraphOptionBA :: TypE
symbolGraphOptionBA = TypE (TS :~> TBA :~> TT :~> TT) GraphOptBA


symbolAxisOptions :: TypE
symbolAxisOptions = TypE (TS :~> TT :~> TT) AxisOpts


symbolAxisOptionI :: TypE
symbolAxisOptionI = TypE (TS :~> TS :~> TI :~> TT :~> TT) AxisOptI

symbolAxisOptionD :: TypE
symbolAxisOptionD = TypE (TS :~> TS :~> TD :~> TT :~> TT) AxisOptD

symbolAxisOptionS :: TypE
symbolAxisOptionS = TypE (TS :~> TS :~> TS :~> TT :~> TT) AxisOptS

symbolAxisOptionB :: TypE
symbolAxisOptionB = TypE (TS :~> TS :~> TB :~> TT :~> TT) AxisOptB

symbolAxisOptionIA :: TypE
symbolAxisOptionIA = TypE (TS :~> TS :~> TIA :~> TT :~> TT) AxisOptIA

symbolAxisOptionDA :: TypE
symbolAxisOptionDA = TypE (TS :~> TS :~> TDA :~> TT :~> TT) AxisOptDA

symbolAxisOptionSA :: TypE
symbolAxisOptionSA = TypE (TS :~> TS :~> TSA :~> TT :~> TT) AxisOptSA

symbolAxisOptionBA :: TypE
symbolAxisOptionBA = TypE (TS :~> TS :~> TBA :~> TT :~> TT) AxisOptBA


symbolSurfaceOptions :: TypE
symbolSurfaceOptions = TypE (TS :~> TT :~> TT) SurfaceOpts


symbolSurfaceOption :: TypE
symbolSurfaceOption = TypE (TS :~> TT :~> TT) SurfaceOpt

symbolSurfaceOptionI :: TypE
symbolSurfaceOptionI = TypE (TS :~> TI :~> TT :~> TT) SurfaceOptI

symbolSurfaceOptionD :: TypE
symbolSurfaceOptionD = TypE (TS :~> TD :~> TT :~> TT) SurfaceOptD

symbolSurfaceOptionS :: TypE
symbolSurfaceOptionS = TypE (TS :~> TS :~> TT :~> TT) SurfaceOptS

symbolSurfaceOptionB :: TypE
symbolSurfaceOptionB = TypE (TS :~> TB :~> TT :~> TT) SurfaceOptB

symbolSurfaceOptionIA :: TypE
symbolSurfaceOptionIA = TypE (TS :~> TIA :~> TT :~> TT) SurfaceOptIA

symbolSurfaceOptionDA :: TypE
symbolSurfaceOptionDA = TypE (TS :~> TDA :~> TT :~> TT) SurfaceOptDA

symbolSurfaceOptionSA :: TypE
symbolSurfaceOptionSA = TypE (TS :~> TSA :~> TT :~> TT) SurfaceOptSA

symbolSurfaceOptionBA :: TypE
symbolSurfaceOptionBA = TypE (TS :~> TBA :~> TT :~> TT) SurfaceOptBA


symbolAddAxis :: TypE
symbolAddAxis = TypE (TS :~> TT :~> TT) $
    SurfaceOpt
    :>>>:
    Apply GraphOptS (CounterN $ TS.pack "axis")

symbolMap :: TypE
symbolMap = TypE ((TT :~> TT) :~> TT :~> TT) Map


symbolWhen :: TypE
symbolWhen = TypE ((TT :~> TB) :~> (TT :~> TT) :~> TT :~> TT) When

symbolWhenElse :: TypE
symbolWhenElse = TypE ((TT :~> TB) :~> (TT :~> TT) :~> (TT :~> TT) :~> TT :~> TT) WhenElse


symbolWhenTransformed :: TypE
symbolWhenTransformed = TypE ((TT :~> TB) :~> (TT :~> TT) :~> (TT :~> TT) :~> TT :~> TT) WhenTransformed

symbolWhenElseTransformed :: TypE
symbolWhenElseTransformed = TypE ((TT :~> TB) :~> (TT :~> TT) :~> (TT :~> TT) :~> (TT :~> TT) :~> TT :~> TT) WhenElseTransformed


symbolHasAnyValue :: TypE
symbolHasAnyValue = TypE ((TD :~> TD :~> TB) :~> TTag TS (TTSC :~> TD) :~> TD :~> TT :~> TB) HasAnyValue

symbolHasAllValue :: TypE
symbolHasAllValue = TypE ((TD :~> TD :~> TB) :~> TTag TS (TTSC :~> TD) :~> TD :~> TT :~> TB) HasAllValue

symbolHasNoValue :: TypE
symbolHasNoValue = TypE ((TD :~> TD :~> TB) :~> TTag TS (TTSC :~> TD) :~> TD :~> TT :~> TB) HasNoValue


symbolHasAnyValueInRange :: TypE
symbolHasAnyValueInRange = TypE ((TD :~> TD :~> TB) :~> TTag TS (TTSC :~> TD) :~> TD :~> TD :~> TT :~> TB) HasAnyValueInRange

symbolHasAllValueInRange :: TypE
symbolHasAllValueInRange = TypE ((TD :~> TD :~> TB) :~> TTag TS (TTSC :~> TD) :~> TD :~> TD :~> TT :~> TB) HasAllValueInRange

symbolHasNoValueInRange :: TypE
symbolHasNoValueInRange = TypE ((TD :~> TD :~> TB) :~> TTag TS (TTSC :~> TD) :~> TD :~> TD :~> TT :~> TB) HasNoValueInRange


symbolGreaterThan :: TypE
symbolGreaterThan = TypE (TD :~> TD :~> TB) GreaterThan

symbolGreaterThanOrEqual :: TypE
symbolGreaterThanOrEqual = TypE (TD :~> TD :~> TB) GreaterThanOrEqual

symbolLessThan :: TypE
symbolLessThan = TypE (TD :~> TD :~> TB) LessThan

symbolLessThanOrEqual :: TypE
symbolLessThanOrEqual = TypE (TD :~> TD :~> TB) LessThanOrEqual


symbolGetAvg :: TypE
symbolGetAvg = TypE (TTag TS (TTSC :~> TD)) GetAvg

symbolGetMin :: TypE
symbolGetMin = TypE (TTag TS (TTSC :~> TD)) GetMin

symbolGetMax :: TypE
symbolGetMax = TypE (TTag TS (TTSC :~> TD)) GetMax

symbolGetCount :: TypE
symbolGetCount = TypE (TTag TS (TTSC :~> TD)) GetCount


symbolNot :: TypE
symbolNot = TypE (TB :~> TB) Not


symbolApplyWithValue :: TypE
symbolApplyWithValue = TypE ((TD :~> TT :~> TT) :~> (TT :~> TD) :~> TT :~> TT) ApplyWithValue

symbolGetLargest :: TypE
symbolGetLargest = TypE (TTag TS (TTSC :~> TD) :~> TT :~> TD) GetLargest

symbolGetSmallest :: TypE
symbolGetSmallest = TypE (TTag TS (TTSC :~> TD) :~> TT :~> TD) GetSmallest

symbolGetLargestInRange :: TypE
symbolGetLargestInRange = TypE (TTag TS (TTSC :~> TD) :~> TD :~> TT :~> TD) GetLargestInRange

symbolGetSmallestInRange :: TypE
symbolGetSmallestInRange = TypE (TTag TS (TTSC :~> TD) :~> TD :~> TT :~> TD) GetSmallestInRange


symbolLargerD :: TypE
symbolLargerD = TypE (TD :~> TD :~> TD) LargerD

symbolSmallerD :: TypE
symbolSmallerD = TypE (TD :~> TD :~> TD) SmallerD

symbolMulD :: TypE
symbolMulD = TypE (TD :~> TD :~> TD) MulD

symbolAddD :: TypE
symbolAddD = TypE (TD :~> TD :~> TD) AddD

symbolDivD :: TypE
symbolDivD = TypE (TD :~> TD :~> TD) DivD

symbolSubD :: TypE
symbolSubD = TypE (TD :~> TD :~> TD) SubD
