{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Language.Paradox.Util (
  typeCheck
, typeCheck'
, exprErrorToReadable
) where

import Text.Printf

import Language.Paradox.Eval.Symbols
import Language.Paradox.Eval.Aliases

import Data.Monoid                              ((<>))
import Data.Either                              ( partitionEithers
                                                , lefts
                                                )
import Data.Attoparsec.ByteString.Char8         ( parseOnly
                                                , option
                                                , char
                                                , many1
                                                , Parser
                                                , digit
                                                , anyChar
                                                )
import Data.Map.Strict                          (Map)

import Language.Paradox.Eval.Types              ( Expr(..)
                                                , Unresolved
                                                , unresolved
                                                , Typ (..)
                                                , TypE (..)
                                                , (:==:)(..)
                                                , ExprError (..)
                                                , equalT
                                                )

import Paradox.Istatd.Types                     (ReturnData)

import qualified Data.Text                      as TS
import qualified Data.ByteString.Char8          as BS
import qualified Data.Map.Strict                as M

import qualified Paradox.Istatd.Types           as IT
import qualified Language.Paradox.Grammar       as G
import qualified Data.List                      as L

data Offset = Pos | Neg




exprErrorToReadable :: ExprError -> String
exprErrorToReadable (ApError f a ft at) =
    let func = printf "Function: %s\nof type %s\n" (show f) (show ft)
        arg = printf "Argument: %s\nof type %s\n" (show a) (show at)
    in concat ["Type mismatch in apply\n", func, arg]
exprErrorToReadable (InfixApError a f at ft) =
    let func = printf "Function: %s\nof type %s\n" (show f) (show ft)
        arg = printf "Argument: %s\nof type %s\n" (show a) (show at)
    in concat ["Type mismatch in InfixApply\n", func, arg]
exprErrorToReadable (SymbolError l) = printf "Bad Symbol Name\nSymbol %s\n" (show l)
exprErrorToReadable (ComposeError f f' fit fot f'it f'ot) =
    let func = printf "Function: %s\nof input type %s\nof output type %s\n" (show f) (show fit) (show fot)
        arg = printf "Argument: %s\nof input type %s\nof output type %s\n" (show f') (show f'it) (show f'ot)
    in concat ["Type mismatch in Compose\n", func, arg]
exprErrorToReadable (UnknownError (l,r)) =
    let left = printf "Left Result: %s\n" (show l)
        right = printf "Right Result: %s\n" (show r)
    in concat ["UnknownError\n", left, right]
exprErrorToReadable (CounterPartialError l) =
    printf "Internal error. Partial counter grammar.\nLit %s\n" (show l)
exprErrorToReadable (CounterArgumentError l) =
    printf "Invalid counter argument literal\nLit %s\n" (show l)
exprErrorToReadable (CounterArgumentParseError e l) =
    printf "Failed to parse counter argument\nParseError: %s\nLit %s\n" (show e) (show l)
exprErrorToReadable (EvaluationError e t) =
    let expression = printf "Expression: %s\n" (show e)
        typ = printf "Type: %s\n" (show t)
    in concat ["Expression cannot be evaluated due to bad final type. Should be TT\n", expression, typ]
exprErrorToReadable (ErrorCollection e) = L.intercalate "\n" $ map exprErrorToReadable e

-- | Converts an expression from 'Language.Paradox.Grammar' into either an
-- error or a type checked 'Language.Paradox.Eval' expression
typeCheck' :: G.Expr
           -> Either ExprError TypE
typeCheck' = \case
    G.CtrLiteral counters
        -> mkCtrs counters
    G.Symbol sym
        -> case lookupAlias sym of
            Just aliasSym
                -> Right aliasSym
            Nothing
                -> lookupFn sym
    G.Ap l r
        -> case (typeCheck' l, typeCheck' r) of
            (Right (TypE tf@(ti :~> to) f), Right (TypE ti' x))
                -> case equalT ti ti' of
                    Just Refl
                        -> Right $ TypE to $ Apply f x
                    Nothing
                        -> Left $ ApError f x tf ti'
            (lv,rv)
                -> Left $ case lefts [lv,rv] of
                            [] -> case (lv,rv) of
                                    (Right (TypE tf f), Right (TypE ti' x)) -> ApError f x tf ti'
                                    e -> UnknownError e
                            ls -> foldr1 (<>) ls
    G.InfixAp l r
        -> case (typeCheck' l, typeCheck' r) of
            (Right (TypE ti' x), Right (TypE tf@(ti :~> to) f ))
                -> case equalT ti ti' of
                    Just Refl
                        -> Right $ TypE to $ x :|: f
                    Nothing
                        -> Left $ InfixApError x f ti' tf
            (lv,rv)
                -> Left $ case lefts [lv,rv] of
                            [] -> case (lv,rv) of
                                    (Right (TypE ti' x), Right (TypE tf f)) -> InfixApError x f ti' tf
                                    e -> UnknownError e
                            ls -> foldr1 (<>) ls
    G.Compose l r
        -> case (typeCheck' l, typeCheck' r) of
            (Right (TypE (ti' :~> to') x), Right (TypE (ti :~> to) f ))
                -> case equalT to' ti of
                    Just Refl
                        -> Right $ TypE (ti' :~> to) $ x :.: f
                    Nothing
                        -> Left $ ComposeError x f ti' to' ti to
            (lv,rv)
                -> Left $ case lefts [lv,rv] of
                            [] -> case (lv,rv) of
                                    (Right (TypE ti' x), Right (TypE tf f)) -> ComposeError x f ti' tf ti' tf
                                    e -> UnknownError e
                            ls -> foldr1 (<>) ls
    G.Literal alit
        -> case alit of
            G.LitString val
                -> Right $ TypE TS $ CounterN $ TS.pack val
            G.LitInt val
                -> Right $ TypE TI $ NumI $ fromIntegral val
            G.LitDouble val
                -> Right $ TypE TD $ NumD val
            G.LitBool val
                -> Right $ TypE TB $ BoolC val
            G.LitIntArray val
                -> Right $ TypE TIA $ NumIA $ map fromIntegral val
            G.LitDoubleArray val
                -> Right $ TypE TDA $ NumDA val
            G.LitStringArray val
                -> Right $ TypE TSA $ StrA $ map TS.pack val
            G.LitBoolArray val
                -> Right $ TypE TBA $ BoolCA val

-- | Converts an expression from 'Language.Paradox.Grammar' into either an
-- error or a type checked 'Language.Paradox.Eval' expression that has not
-- been resolved yet (since that requires some possibly impure code)
typeCheck :: G.Expr
          -> Either ExprError (Unresolved (Expr ReturnData) )
typeCheck = evaluable . typeCheck'

evaluable :: Either ExprError TypE
          -> Either ExprError (Unresolved (Expr ReturnData) )
evaluable = \case
    Right (TypE t e) -> case t of
        TT -> Right $ unresolved e
        _ -> Left $ EvaluationError e t
    Left err -> Left err



mkCtrs :: G.SpecdCounters
       -> Either ExprError TypE
mkCtrs (G.SpecdCounters ctrs ps) = case partitionEithers $ map (mkCtr ps) ctrs of
    ([], counters) -> Right $ TypE TT $ CounterIntermediate counters
    (errs, _) -> Left $ ErrorCollection errs

mkCtr :: Maybe (Map String G.Lit)
      -> G.CtrParts
      -> Either ExprError IT.CounterSpecIntermediate
mkCtr p ctr = case getOffsetParam p of
    Nothing -> case partitionEithers $ map unglob ctr of
                 ([], counters) -> Right $ IT.CounterSpecIntermediate counters Nothing
                 (errs, _) -> Left $ ErrorCollection errs
    (Just (Right offsetVal)) -> case partitionEithers $ map unglob ctr of
                                  ([], counters) -> Right $ IT.CounterSpecIntermediate counters (Just offsetVal)
                                  (errs, _) -> Left $ ErrorCollection errs
    (Just (Left err)) -> Left err
    where
        unglob = \case
            (G.CtrLit lit) -> Right $ IT.CNPString $ TS.pack lit
            G.CtrGlob -> Right IT.CNPGlobStar
            G.CtrQuestion -> Right IT.CNPGlobQuestion
            G.CtrDot -> Right IT.CNPDot
            (G.CtrShell lit) -> Right $ IT.CNPShellReplacement $ TS.pack lit
            a@(G.CtrArray _) -> Left $ CounterPartialError a

getOffsetParam :: Maybe (Map String G.Lit)
               -> Maybe (Either ExprError Int)
getOffsetParam = \case
    Nothing -> Nothing
    Just params -> getOffsetParam' params

getOffsetParam' :: Map String G.Lit
                -> Maybe (Either ExprError Int)
getOffsetParam' ps = counterParam
    where
        counterParam = coerceToInt <$> M.lookup "offset" ps --Maybe support more params?

coerceToInt :: G.Lit
            -> Either ExprError Int
coerceToInt = \case
    G.LitInt lit -> Right $ fromInteger lit
    G.LitDouble lit -> Right $ fromInteger (floor lit)
    l'@(G.LitString lit) -> case parseOnly parseOffset $ BS.pack lit of
        (Left innerErr) -> Left $ CounterArgumentParseError innerErr l'
        (Right v) -> Right v
    invalid -> Left $ CounterArgumentError invalid

parseOffset :: Parser Int
parseOffset = do
    neg <- option Pos (char '-' >> return Neg)
    vals <- many1 parseOffsetTerm
    return $ factor neg * sum vals
    where
        factor = \case
            Pos -> 1
            Neg -> (-1)

parseOffsetTerm :: Parser Int
parseOffsetTerm = do
    digits <- many1 digit
    factorChar <- anyChar
    maybe
        (fail $ "Bad Factor" ++ show factorChar)
        (\factor -> return $ (read digits :: Int) * factor )
        (lookupFactor factorChar)

lookupFactor :: Char
             -> Maybe Int
lookupFactor = \case
    's' -> Just s
    'm' -> Just m
    'h' -> Just h
    'D' -> Just d
    'W' -> Just w
    'M' -> Just mo
    'Y' -> Just y
    _ -> Nothing
    where
        s = 1
        m = 60
        h = 60 * m
        d = 24 * h
        w = 7 * d
        mo = 30 * d
        y = 365 * d

lookupAlias :: String
            -> Maybe TypE
lookupAlias "dyColor" = Just aliasDyColor
lookupAlias "dyColorSaturation" = Just aliasDyColorSaturation
lookupAlias "dyColorValue" = Just aliasDyColorValue
lookupAlias "dyStrokeBorderColor" = Just aliasDyStrokeBorderColor
lookupAlias "dyStrokeWidth" = Just aliasDyStrokeWidth
lookupAlias "dyStrokeBorderWidth" = Just aliasDyStrokeBorderWidth
lookupAlias "dyStrokePattern" = Just aliasDyStrokePattern
lookupAlias "dyPointSize" = Just aliasDyPointSize
lookupAlias "dyFillGraph" = Just aliasDyFillGraph
lookupAlias "dyFillAlpha" = Just aliasDyFillAlpha
lookupAlias "dyAxis" = Just aliasDyAxis
lookupAlias "dyDrawPoints" = Just aliasDyDrawPoints
lookupAlias "dyConnectSeparatedPoints" = Just aliasDyConnectSeparatedPoints
lookupAlias "dyShowAnnotations" = Just aliasDyShowAnnotations
lookupAlias "dyDrawGapEdgePoints" = Just aliasDyDrawGapEdgePoints
lookupAlias "dyStackedGraph" = Just aliasDyStackedGraph
lookupAlias "dyStackedGraphNaNFill" = Just aliasDyStackedGraphNaNFill
lookupAlias "dyAxisLogScaleY" = Just aliasDyAxisLogScaleY
lookupAlias "dyAxisLogScaleY2" = Just aliasDyAxisLogScaleY2
lookupAlias "dyAxisIndependentTicksY" = Just aliasDyAxisIndependentTicksY
lookupAlias "dyAxisIndependentTicksY2" = Just aliasDyAxisIndependentTicksY2
lookupAlias "dyAxisDrawAxisY" = Just aliasDyAxisDrawAxisY
lookupAlias "dyAxisDrawAxisY2" = Just aliasDyAxisDrawAxisY2
lookupAlias "dyAxisIncludeZeroY" = Just aliasDyAxisIncludeZeroY
lookupAlias "dyAxisIncludeZeroY2" = Just aliasDyAxisIncludeZeroY2
lookupAlias "dyGraphNulls" = Just aliasDyGraphNulls
lookupAlias "dyStepPlot" = Just aliasDyStepPlot
lookupAlias "dyForceStep" = Just aliasDyForceStep
lookupAlias "dyStepNulls" = Just aliasDyStepNulls
lookupAlias "dyAddAxis" = Just addAxis
lookupAlias "dyConstrainMin" = Just aliasDyConstrainMin
lookupAlias "dyConstrainMax" = Just aliasDyConstrainMax
lookupAlias "dyConstrainMinY2" = Just aliasDyConstrainMinY2
lookupAlias "dyConstrainMaxY2" = Just aliasDyConstrainMaxY2
lookupAlias "dyConstrain" = Just aliasDyConstrain
lookupAlias "dyConstrainY2" = Just aliasDyConstrainY2
lookupAlias "dyRemoveMinConstraint" = Just aliasDyRemoveMinConstraint
lookupAlias "dyRemoveMaxConstraint" = Just aliasDyRemoveMaxConstraint
lookupAlias "dyRemoveMinConstraintY2" = Just aliasDyRemoveMinConstraintY2
lookupAlias "dyRemoveMaxConstraintY2" = Just aliasDyRemoveMaxConstraintY2
lookupAlias "dyRemoveConstraint" = Just aliasDyRemoveConstraint
lookupAlias "dyRemoveConstraintY2" = Just aliasDyRemoveConstraintY2
lookupAlias _ = Nothing

addAxis :: TypE
addAxis = symbolAddAxis

lookupFn :: String
         -> Either ExprError TypE
lookupFn "sum" = Right symbolSum
lookupFn "add" = Right symbolSum
lookupFn "maxMaxAtEachSample" = Right symbolMaxMaxAtEachSample
lookupFn "maxAvgAtEachSample" = Right symbolMaxAvgAtEachSample
lookupFn "maxMinAtEachSample" = Right symbolMaxMinAtEachSample
lookupFn "maxValueAtEachSample" = Right symbolMaxValueAtEachSample
lookupFn "graphCount" = Right symbolGraphCount
lookupFn "graphMax" = Right symbolGraphMax
lookupFn "graphMin" = Right symbolGraphMin
lookupFn "graphAvg" = Right symbolGraphAvg
lookupFn "graphField" = Right symbolGraphField
lookupFn "summarizeAvg" = Right symbolSummarizeAvg
lookupFn "summarizeSum" = Right symbolSummarizeSum
lookupFn "avg" = Right symbolAvg
lookupFn "average" = Right symbolAvg
lookupFn "abs" = Right symbolAbs
lookupFn "absoluteValue" = Right symbolAbsoluteValue
lookupFn "movingAverage" = Right symbolMovingAverage
lookupFn "medianFilter" = Right symbolMedianFilter
lookupFn "medianAbsoluteDeviationFilter" = Right symbolMedianAbsoluteDeviationFilter
lookupFn "medianAbsoluteDeviation" = Right symbolMedianAbsoluteDeviation
lookupFn "doubleMedianAbsoluteDeviationFilter" = Right symbolDoubleMedianAbsoluteDeviationFilter
lookupFn "doubleMedianAbsoluteDeviation" = Right symbolDoubleMedianAbsoluteDeviation
lookupFn "loess" = Right symbolLoess
lookupFn "edmMulti" = Right symbolEDMMulti
lookupFn "stl" = Right symbolStl
lookupFn "stlSeason" = Right symbolStlSeason
lookupFn "stlTrend" = Right symbolStlTrend
lookupFn "stlResidual" = Right symbolStlResidual
lookupFn "offset" = Right symbolOffset
lookupFn "keepSeriesAboveDeviation" = Right symbolKeepSeriesAboveDeviation
lookupFn "keepSeriesAboveDeviationInc" = Right symbolKeepSeriesAboveDeviationInc
lookupFn "keepSeriesWithinDeviation" = Right symbolKeepSeriesWithinDeviation
lookupFn "keepSeriesWithinDeviationInc" = Right symbolKeepSeriesWithinDeviationInc
lookupFn "removeAbovePercentile" = Right symbolRemoveAbovePercentile
lookupFn "removeAbovePercentileInc" = Right symbolRemoveAbovePercentileInc
lookupFn "removeBelowPercentile" = Right symbolRemoveBelowPercentile
lookupFn "removeBelowPercentileInc" = Right symbolRemoveBelowPercentileInc
lookupFn "removeAboveValue" = Right symbolRemoveAboveValue
lookupFn "removeAboveValueInc" = Right symbolRemoveAboveValueInc
lookupFn "removeBelowValue" = Right symbolRemoveBelowValue
lookupFn "removeBelowValueInc" = Right symbolRemoveBelowValueInc
lookupFn "ceiling" = Right symbolCeiling
lookupFn "floor" = Right symbolFloor
lookupFn "keepSeriesAbove" = Right symbolKeepSeriesAbove
lookupFn "keepSeriesAboveInc" = Right symbolKeepSeriesAboveInc
lookupFn "keepSeriesBelow" = Right symbolKeepSeriesBelow
lookupFn "keepSeriesBelowInc" = Right symbolKeepSeriesBelowInc
lookupFn "keepSeriesAllAbove" = Right symbolKeepSeriesAllAbove
lookupFn "keepSeriesAllAboveInc" = Right symbolKeepSeriesAllAboveInc
lookupFn "keepSeriesAllBelow" = Right symbolKeepSeriesAllBelow
lookupFn "keepSeriesAllBelowInc" = Right symbolKeepSeriesAllBelowInc
lookupFn "alias" = Right symbolAlias
lookupFn "smartAlias" = Right symbolSmartAlias
lookupFn "smartAliasA" = Right symbolSmartAliasA
lookupFn "exclude" = Right symbolExclude
lookupFn "include" = Right symbolInclude
lookupFn "scale" = Right symbolScale
lookupFn "scalarDivide" = Right symbolScalarDivide
lookupFn "scaleByInterval" = Right symbolScaleByInterval
lookupFn "divideByInterval" = Right symbolDivideByInterval
lookupFn "integrate" = Right symbolIntegrate
lookupFn "derive" = Right symbolDerive
lookupFn "log" = Right symbolLog
lookupFn "sub" = Right symbolSub
lookupFn "subtract" = Right symbolSubtract
lookupFn "divide" = Right symbolDivide
lookupFn "cons" = Right symbolCons
lookupFn "constantLine" = Right symbolConstantLine
lookupFn "graphOptions" = Right symbolGraphOptions

lookupFn "graphOptionI" = Right symbolGraphOptionI
lookupFn "graphOptionD" = Right symbolGraphOptionD
lookupFn "graphOptionS" = Right symbolGraphOptionS
lookupFn "graphOptionB" = Right symbolGraphOptionB
lookupFn "graphOptionIA" = Right symbolGraphOptionIA
lookupFn "graphOptionDA" = Right symbolGraphOptionDA
lookupFn "graphOptionSA" = Right symbolGraphOptionSA
lookupFn "graphOptionBA" = Right symbolGraphOptionBA

lookupFn "axisOptions" = Right symbolAxisOptions

lookupFn "axisOptionI" = Right symbolAxisOptionI
lookupFn "axisOptionD" = Right symbolAxisOptionD
lookupFn "axisOptionS" = Right symbolAxisOptionS
lookupFn "axisOptionB" = Right symbolAxisOptionB
lookupFn "axisOptionIA" = Right symbolAxisOptionIA
lookupFn "axisOptionDA" = Right symbolAxisOptionDA
lookupFn "axisOptionSA" = Right symbolAxisOptionSA
lookupFn "axisOptionBA" = Right symbolAxisOptionBA

lookupFn "surfaceOptions" = Right symbolSurfaceOptions

lookupFn "surfaceOption" = Right symbolSurfaceOption
lookupFn "surfaceOptionI" = Right symbolSurfaceOptionI
lookupFn "surfaceOptionD" = Right symbolSurfaceOptionD
lookupFn "surfaceOptionS" = Right symbolSurfaceOptionS
lookupFn "surfaceOptionB" = Right symbolSurfaceOptionB
lookupFn "surfaceOptionIA" = Right symbolSurfaceOptionIA
lookupFn "surfaceOptionDA" = Right symbolSurfaceOptionDA
lookupFn "surfaceOptionSA" = Right symbolSurfaceOptionSA
lookupFn "surfaceOptionBA" = Right symbolSurfaceOptionBA

lookupFn "addAxis" = Right addAxis

lookupFn "map" = Right symbolMap

lookupFn "when" = Right symbolWhen
lookupFn "whenElse" = Right symbolWhenElse

lookupFn "whenTransformed" = Right symbolWhenTransformed
lookupFn "whenElseTransformed" = Right symbolWhenElseTransformed

lookupFn "hasAnyValue" = Right symbolHasAnyValue
lookupFn "hasAllValue" = Right symbolHasAllValue
lookupFn "hasNoValue" = Right symbolHasNoValue

lookupFn "hasAnyValueInRange" = Right symbolHasAnyValueInRange
lookupFn "hasAllValueInRange" = Right symbolHasAllValueInRange
lookupFn "hasNoValueInRange" = Right symbolHasNoValueInRange

lookupFn "greaterThan" = Right symbolGreaterThan
lookupFn "greaterThanOrEqual" = Right symbolGreaterThanOrEqual
lookupFn "lessThan" = Right symbolLessThan
lookupFn "lessThanOrEqual" = Right symbolLessThanOrEqual

lookupFn "getAvg" = Right symbolGetAvg
lookupFn "getMin" = Right symbolGetMin
lookupFn "getMax" = Right symbolGetMax
lookupFn "getCount" = Right symbolGetCount

lookupFn "not" = Right symbolNot

lookupFn "applyWithValue" = Right symbolApplyWithValue
lookupFn "getLargest" = Right symbolGetLargest
lookupFn "getSmallest" = Right symbolGetSmallest
lookupFn "getLargestInRange" = Right symbolGetLargestInRange
lookupFn "getSmallestInRange" = Right symbolGetSmallestInRange

lookupFn "largerD" = Right symbolLargerD
lookupFn "smallerD" = Right symbolSmallerD
lookupFn "mulD" = Right symbolMulD
lookupFn "addD" = Right symbolAddD
lookupFn "divD" = Right symbolDivD
lookupFn "subD" = Right symbolSubD

lookupFn "stringArrayTest" = Right $ TypE (TSA :~> TT :~> TT) StringArrayTest
lookupFn "intArrayTest" = Right $ TypE (TIA :~> TT :~> TT) IntArrayTest
lookupFn "doubleArrayTest" = Right $ TypE (TDA :~> TT :~> TT) DoubleArrayTest

lookupFn err = Left $ SymbolError err
