{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Paradox.Eval.Types
( World(..)
, WorldConstraints(..)
, resolved
, unresolved
, Unresolved
, Expr (..)
, Accum (..)
, LogEntry (..)
, ExprType (..)
, (:==:) (..)
, Typ (..)
, equalT
, TypE (..)
, ExprError (..)
, TypeMatch (..)
, MatchCollection (..)
)
where

import Data.Aeson                               ( ToJSON(..)
                                                , (.=)
                                                , object
                                                )
import Data.Default
import Data.Monoid                              ( (<>) )
import Data.Typeable                            ( Typeable
                                                , typeOf
                                                )
import Paradox.Functions.Util                   ( Summarizer )
import Paradox.Istatd.CounterMap                ( CounterMap )
import Paradox.Istatd.Types                     ( TimeRange
                                                , ParadoxTimeSeries
                                                , ReturnData
                                                , TimeSeriesChunk
                                                , CounterSpec
                                                , CounterSpecIntermediate(..)
                                                , CounterName
                                                )


import qualified Data.ByteString                as BS
import qualified Data.Text                      as TS
import qualified Language.Paradox.Grammar       as G


data World
  = World
  { wMap :: CounterMap ParadoxTimeSeries
  , wConstraints :: WorldConstraints
  }

data WorldConstraints
  = WorldConstraints
  { wTimeRange :: TimeRange
  , wMaxSamples :: Int
  , wInterval :: Maybe Int
  }

instance Default World where
  def = World
      { wMap = def
      , wConstraints = def
      }

instance Default WorldConstraints where
  def = WorldConstraints
      { wTimeRange = def
      , wMaxSamples = 0
      , wInterval = Just 0
      }

-- | The expression type of a counter query AST.
data Expr a where
  Id
    :: Expr (a -> a)
  Sum
    :: Expr (ReturnData -> ReturnData)
  GraphField
    :: Expr ((TS.Text, TimeSeriesChunk -> Double) -> ReturnData -> ReturnData)
  MaxAtEachSample
    :: Expr ((TS.Text, TimeSeriesChunk -> Double) -> ReturnData -> ReturnData)
  Summarize
    :: Summarizer
    -> Expr (Int -> ReturnData -> ReturnData)
  Avg
    :: Expr (ReturnData -> ReturnData)
  Abs
    :: Expr (ReturnData -> ReturnData)
  Offset
    :: Expr (Double -> ReturnData -> ReturnData)
  KeepSeriesAboveDeviation
    :: Expr (Int -> ReturnData -> ReturnData)
  KeepSeriesAboveDeviationInc
    :: Expr (Int -> ReturnData -> ReturnData)
  KeepSeriesWithinDeviation
    :: Expr (Int -> ReturnData -> ReturnData)
  KeepSeriesWithinDeviationInc
    :: Expr (Int -> ReturnData -> ReturnData)
  RemoveAbovePercentile
    :: Expr (Int -> ReturnData -> ReturnData)
  RemoveAbovePercentileInc
    :: Expr (Int -> ReturnData -> ReturnData)
  RemoveBelowPercentile
    :: Expr (Int -> ReturnData -> ReturnData)
  RemoveBelowPercentileInc
    :: Expr (Int -> ReturnData -> ReturnData)
  RemoveAboveValue
    :: Expr (Double -> ReturnData -> ReturnData)
  RemoveAboveValueInc
    :: Expr (Double -> ReturnData -> ReturnData)
  RemoveBelowValue
    :: Expr (Double -> ReturnData -> ReturnData)
  RemoveBelowValueInc
    :: Expr (Double -> ReturnData -> ReturnData)
  Ceiling
    :: Expr (Double -> ReturnData -> ReturnData)
  Floor
    :: Expr (Double -> ReturnData -> ReturnData)
  KeepSeriesAbove
    :: Expr (Double -> ReturnData -> ReturnData)
  KeepSeriesAboveInc
    :: Expr (Double -> ReturnData -> ReturnData)
  KeepSeriesBelow
    :: Expr (Double -> ReturnData -> ReturnData)
  KeepSeriesBelowInc
    :: Expr (Double -> ReturnData -> ReturnData)
  KeepSeriesAllAbove
    :: Expr (Double -> ReturnData -> ReturnData)
  KeepSeriesAllAboveInc
    :: Expr (Double -> ReturnData -> ReturnData)
  KeepSeriesAllBelow
    :: Expr (Double -> ReturnData -> ReturnData)
  KeepSeriesAllBelowInc
    :: Expr (Double -> ReturnData -> ReturnData)
  MovingAverage
    :: Expr (Int -> ReturnData -> ReturnData)
  MedianFilter
    :: Expr (Int -> ReturnData -> ReturnData)
  MedianAbsoluteDeviation
    :: Expr (ReturnData -> ReturnData)
  MedianAbsoluteDeviationFilter
    :: Expr (Int -> ReturnData -> ReturnData)
  DoubleMedianAbsoluteDeviation
    :: Expr (ReturnData -> ReturnData)
  DoubleMedianAbsoluteDeviationFilter
    :: Expr (Int -> ReturnData -> ReturnData)
  Loess
    :: Expr (Double -> ReturnData -> ReturnData)
  EDMMulti
    :: Expr (Int -> Double -> ReturnData -> ReturnData)
  Stl
    :: Expr (ReturnData -> ReturnData)
  StlSeason
    :: Expr (ReturnData -> ReturnData)
  StlTrend
    :: Expr (ReturnData -> ReturnData)
  StlResidual
    :: Expr (ReturnData -> ReturnData)
  ConstantLine
    :: Expr (Double -> ReturnData)
  Sub
    :: Expr (ReturnData -> ReturnData -> ReturnData)
  Divide
    :: Expr (ReturnData -> ReturnData -> ReturnData)
  Cons
    :: Expr (ReturnData -> ReturnData -> ReturnData)
  Alias
    :: Expr (CounterName -> ReturnData -> ReturnData)
  SmartAlias
    :: Expr (CounterName -> Int -> ReturnData -> ReturnData)
  SmartAliasA
    :: Expr (CounterName -> [Int] -> ReturnData -> ReturnData)
  Exclude
    :: Expr (CounterName -> ReturnData -> ReturnData)
  Include
    :: Expr (CounterName -> ReturnData -> ReturnData)
  Scale
    :: Expr (Double -> ReturnData -> ReturnData)
  ScalarDivide
    :: Expr (Double -> ReturnData -> ReturnData)
  ScaleByInterval
    :: Expr (ReturnData -> ReturnData)
  DivideByInterval
    :: Expr (ReturnData -> ReturnData)
  Integrate
    :: Expr (ReturnData -> ReturnData)
  Derive
    :: Expr (ReturnData -> ReturnData)
  Log
    :: Expr (Int -> Double -> Int -> ReturnData -> ReturnData)

  GraphOpts
    :: Expr (TS.Text -> ReturnData -> ReturnData)

  GraphOptI
    :: Expr (TS.Text -> Int -> ReturnData -> ReturnData)
  GraphOptD
    :: Expr (TS.Text -> Double -> ReturnData -> ReturnData)
  GraphOptS
    :: Expr (TS.Text -> TS.Text -> ReturnData -> ReturnData)
  GraphOptB
    :: Expr (TS.Text -> Bool -> ReturnData -> ReturnData)
  GraphOptIA
    :: Expr (TS.Text -> [Int] -> ReturnData -> ReturnData)
  GraphOptDA
    :: Expr (TS.Text -> [Double] -> ReturnData -> ReturnData)
  GraphOptSA
    :: Expr (TS.Text -> [TS.Text] -> ReturnData -> ReturnData)
  GraphOptBA
    :: Expr (TS.Text -> [Bool] -> ReturnData -> ReturnData)

  AxisOpts
    :: Expr (TS.Text -> ReturnData -> ReturnData)

  AxisOptI
    :: Expr (TS.Text -> TS.Text -> Int -> ReturnData -> ReturnData)
  AxisOptD
    :: Expr (TS.Text -> TS.Text -> Double -> ReturnData -> ReturnData)
  AxisOptS
    :: Expr (TS.Text -> TS.Text -> TS.Text -> ReturnData -> ReturnData)
  AxisOptB
    :: Expr (TS.Text -> TS.Text -> Bool -> ReturnData -> ReturnData)
  AxisOptIA
    :: Expr (TS.Text -> TS.Text -> [Int] -> ReturnData -> ReturnData)
  AxisOptDA
    :: Expr (TS.Text -> TS.Text -> [Double] -> ReturnData -> ReturnData)
  AxisOptSA
    :: Expr (TS.Text -> TS.Text -> [TS.Text] -> ReturnData -> ReturnData)
  AxisOptBA
    :: Expr (TS.Text -> TS.Text -> [Bool] -> ReturnData -> ReturnData)

  SurfaceOpts
    :: Expr (TS.Text -> ReturnData -> ReturnData)

  SurfaceOpt
    :: Expr (TS.Text -> ReturnData -> ReturnData)
  SurfaceOptI
    :: Expr (TS.Text -> Int -> ReturnData -> ReturnData)
  SurfaceOptD
    :: Expr (TS.Text -> Double -> ReturnData -> ReturnData)
  SurfaceOptS
    :: Expr (TS.Text -> TS.Text -> ReturnData -> ReturnData)
  SurfaceOptB
    :: Expr (TS.Text -> Bool -> ReturnData -> ReturnData)
  SurfaceOptIA
    :: Expr (TS.Text -> [Int] -> ReturnData -> ReturnData)
  SurfaceOptDA
    :: Expr (TS.Text -> [Double] -> ReturnData -> ReturnData)
  SurfaceOptSA
    :: Expr (TS.Text -> [TS.Text] -> ReturnData -> ReturnData)
  SurfaceOptBA
    :: Expr (TS.Text -> [Bool] -> ReturnData -> ReturnData)

  StringArrayTest
    :: Expr ([TS.Text] -> ReturnData -> ReturnData)
  IntArrayTest
    :: Expr ([Int] -> ReturnData -> ReturnData)
  DoubleArrayTest
    :: Expr ([Double] -> ReturnData -> ReturnData)

  (:|:)
    :: Expr a
    -> Expr (a -> b)
    -> Expr b
  Apply
    :: Expr (a -> b)
    -> Expr a 
    -> Expr b
  Counter
    :: [CounterSpec]
    -> Expr ReturnData
  CounterIntermediate
    :: [CounterSpecIntermediate]
    -> Expr ReturnData
  CounterN
    :: CounterName
    -> Expr CounterName
  NumD
    :: Double
    -> Expr Double
  NumI
    :: Int
    -> Expr Int
  BoolC
    :: Bool
    -> Expr Bool
  NumIA
    :: [Int] -> Expr [Int]
  NumDA
    :: [Double] -> Expr [Double]
  BoolCA
    :: [Bool] -> Expr [Bool]
  StrA
    :: [TS.Text] -> Expr [TS.Text]

  (:.:)
    :: Expr (a -> b)
    -> Expr (b -> c)
    -> Expr (a -> c)
  (:>>>:)
    :: Expr (a -> b -> c)
    -> Expr (a -> c -> d)
    -> Expr (a -> b -> d)
  (:...:)
    :: Expr (a -> b -> c)
    -> Expr (d -> c -> e)
    -> Expr (a -> d -> b -> e)
  Map
    :: Expr ((ReturnData -> ReturnData) -> ReturnData -> ReturnData)

  When
    :: Expr ((ReturnData -> Bool) -> (ReturnData -> ReturnData) -> ReturnData -> ReturnData)
  WhenElse
    :: Expr ((ReturnData -> Bool) -> (ReturnData -> ReturnData) -> (ReturnData -> ReturnData) -> ReturnData -> ReturnData)

  WhenTransformed
    :: Expr ((ReturnData -> Bool) -> (ReturnData -> ReturnData) -> (ReturnData -> ReturnData) -> ReturnData -> ReturnData)

  WhenElseTransformed
    :: Expr ((ReturnData -> Bool) -> (ReturnData -> ReturnData) -> (ReturnData -> ReturnData) -> (ReturnData -> ReturnData) -> ReturnData -> ReturnData)

  HasAnyValue
    :: Expr ((Double -> Double -> Bool) -> (TS.Text, TimeSeriesChunk -> Double) -> Double -> ReturnData -> Bool)
  HasAllValue
    :: Expr ((Double -> Double -> Bool) -> (TS.Text, TimeSeriesChunk -> Double) -> Double -> ReturnData -> Bool)
  HasNoValue
    :: Expr ((Double -> Double -> Bool) -> (TS.Text, TimeSeriesChunk -> Double) -> Double -> ReturnData -> Bool)

  HasAnyValueInRange
    :: Expr ((Double -> Double -> Bool) -> (TS.Text, TimeSeriesChunk -> Double) -> Double -> Double -> ReturnData -> Bool)
  HasAllValueInRange
    :: Expr ((Double -> Double -> Bool) -> (TS.Text, TimeSeriesChunk -> Double) -> Double -> Double -> ReturnData -> Bool)
  HasNoValueInRange
    :: Expr ((Double -> Double -> Bool) -> (TS.Text, TimeSeriesChunk -> Double) -> Double -> Double -> ReturnData -> Bool)


  GreaterThan
    :: Expr (Double -> Double -> Bool)
  GreaterThanOrEqual
    :: Expr (Double -> Double -> Bool)
  LessThan
    :: Expr (Double -> Double -> Bool)
  LessThanOrEqual
    :: Expr (Double -> Double -> Bool)

  GetAvg
    :: Expr (TS.Text, TimeSeriesChunk -> Double)
  GetMin
    :: Expr (TS.Text, TimeSeriesChunk -> Double)
  GetMax
    :: Expr (TS.Text, TimeSeriesChunk -> Double)
  GetCount
    :: Expr (TS.Text, TimeSeriesChunk -> Double)

  Not
    :: Expr (Bool -> Bool)

  ApplyWithValue
    :: Expr ((Double -> ReturnData -> ReturnData) -> (ReturnData -> Double) -> ReturnData -> ReturnData)

  GetLargest
    :: Expr ((TS.Text, TimeSeriesChunk -> Double) -> ReturnData -> Double)
  GetSmallest
    :: Expr ((TS.Text, TimeSeriesChunk -> Double) -> ReturnData -> Double)
  GetLargestInRange
    :: Expr ((TS.Text, TimeSeriesChunk -> Double) -> Double -> ReturnData -> Double)
  GetSmallestInRange
    :: Expr ((TS.Text, TimeSeriesChunk -> Double) -> Double -> ReturnData -> Double)

  SmallerD
    :: Expr (Double -> Double -> Double)
  LargerD
    :: Expr (Double -> Double -> Double)
  MulD
    :: Expr (Double -> Double -> Double)
  AddD
    :: Expr (Double -> Double -> Double)
  DivD
    :: Expr (Double -> Double -> Double)
  SubD
    :: Expr (Double -> Double -> Double)
  deriving Typeable

deriving instance Show (Expr a)

instance (Typeable a, Typeable b) => Show (a->b) where
  show _ = show $ typeOf (undefined :: a -> b)

data ExprType
  = Func
  | Bind
  | Lit
  | Pass
  | HigherOrder
  deriving (Eq, Enum, Bounded, Show)

data LogEntry
  = LogEntry
  { logEntry :: BS.ByteString
  , exprType :: ExprType
  } deriving (Eq, Show)

newtype Accum
  = Accum { unLogs :: [LogEntry] }
  deriving (Eq, Show, Monoid)

data Unresolved a
  = Unresolved a


instance Show a => Show (Unresolved a) where
  show (Unresolved a) = "Unresolved" ++ show a

data TypeMatch
  = PartialMatch MatchCollection
  | FullMatch MatchCollection
  | AllMatch
  | NoneMatch

data MatchCollection
  = forall a. Typeable a => MatchCollection (Typ a)

instance Show MatchCollection where
  show (MatchCollection t) = show t

instance Show TypeMatch where
  show (PartialMatch ms) = "PartialMatch " ++ show ms
  show (FullMatch ms) = "FullMatch " ++ show ms
  show NoneMatch = "NoneMatch"
  show AllMatch = "AllMatch"

infixr 9 :~>

-- | Typed expression type token
data Typ a where
  TT
    :: Typ ReturnData
  TTSC
    :: Typ TimeSeriesChunk
  TTag
    :: ( Typeable a
       , Typeable b
       )
    => Typ a
    -> Typ b
    -> Typ (a, b)
  TS
    :: Typ CounterName
  TD
    :: Typ Double
  TI
    :: Typ Int
  TB
    :: Typ Bool
  TIA
    :: Typ [Int]
  TDA
    :: Typ [Double]
  TSA
    :: Typ [TS.Text]
  TBA
    :: Typ [Bool]
  (:~>)
    :: ( Typeable a
       , Typeable b
       )
    => Typ a
    -> Typ b
    -> Typ (a->b)
  deriving (Typeable)

deriving instance Show (Typ a)

-- | Check the equality of @a@ and @b@a
equalT
  :: Typ a
  -> Typ b
  -> Maybe (a :==: b)
equalT x y = case (x,y) of
  (TT, TT)                    -> Just Refl
  (TS, TS)                    -> Just Refl
  (TD, TD)                    -> Just Refl
  (TB, TB)                    -> Just Refl
  (TI, TI)                    -> Just Refl
  (TIA, TIA)                  -> Just Refl
  (TDA, TDA)                  -> Just Refl
  (TSA, TSA)                  -> Just Refl
  (TBA, TBA)                  -> Just Refl
  (TTSC, TTSC)                -> Just Refl
  (TTag a b, TTag a' b')      -> case (equalT a a', equalT b b') of
      (Just Refl, Just Refl)      -> Just Refl
      _                           -> Nothing
  (a :~> b, a' :~> b')        -> case (equalT a a', equalT b b') of
      (Just Refl, Just Refl)      -> Just Refl
      _                           -> Nothing
  (_, _)                          -> Nothing

-- | Typed expression with its type token, existentially quantified
data TypE
  = forall a. Typeable a => TypE (Typ a) (Expr a)
  deriving Typeable

deriving instance Show TypE

-- | Proves the equality of two types by construction
data a :==: b where
  Refl :: a :==: a


data ExprError where
  ApError
    :: Expr a
    -> Expr b
    -> Typ a'
    -> Typ b'
    -> ExprError
  InfixApError
    :: Expr a
    -> Expr b
    -> Typ a'
    -> Typ b'
    -> ExprError
  SymbolError
    :: String
    -> ExprError
  ComposeError
    :: Expr a
    -> Expr b
    -> Typ a'
    -> Typ c'
    -> Typ b'
    -> Typ d'
    -> ExprError
  UnknownError
    :: (Either ExprError TypE, Either ExprError TypE)
    -> ExprError
  CounterPartialError
    :: G.CtrPart
    -> ExprError
  CounterArgumentError
    :: G.Lit
    -> ExprError
  CounterArgumentParseError
    :: String
    -> G.Lit
    -> ExprError
  EvaluationError
    :: Expr a
    -> Typ a
    -> ExprError
  ErrorCollection
    :: [ExprError]
    -> ExprError

deriving instance Show ExprError

instance Monoid ExprError where
  mempty
    = ErrorCollection []
  mappend (ErrorCollection e) (ErrorCollection e')
    = ErrorCollection $ e <> e'
  mappend (ErrorCollection e) r
    = ErrorCollection $ e ++ [r]
  mappend l (ErrorCollection e)
    = ErrorCollection $ e ++ [l]
  mappend l r
    = ErrorCollection [l,r]

instance ToJSON ExprError where
  toJSON (ApError func arg funcT argT)
    = object
    [ "error" .= object
      [ "function" .= show func
      , "functionType" .= show funcT
      , "argument" .= show arg
      , "argumentType" .= show argT
      ]
    , "message" .= ("Type mismatch in apply" :: TS.Text)
    ]
  toJSON (InfixApError arg func argT funcT)
    = object
    [ "error" .= object
      [ "function" .= show func
      , "functionType" .= show funcT
      , "argument" .= show arg
      , "argumentType" .= show argT
      ]
    , "message" .= ("Type mismatch in infixApply" :: TS.Text)
    ]
  toJSON (SymbolError sym)
    = object
    [ "error" .= object
      [ "symbolName" .= show sym ]
    , "message" .= ("Bad symbol name" :: TS.Text)
    ]
  toJSON (ComposeError left right lIn lOut rIn rOut)
    = object
    [ "error" .= object
      [ "leftExpr" .= show left
      , "rightExpr" .= show right
      , "leftArgType" .= show lIn
      , "leftReturnType" .= show lOut
      , "rightArgType" .= show rIn
      , "rightReturnType" .= show rOut
      ]
    , "message" .= ("Type mismatch in compose" :: TS.Text)
    ]
  toJSON (UnknownError (leftR,rightR))
    = object
    [ "error" .= object
      [ "leftResult" .= show leftR
      , "rightResult" .= show rightR
      ]
    , "message" .= ("Unknown error" :: TS.Text)
    ]
  toJSON (CounterPartialError arg)
    = object
    [ "error" .= object
      [ "argument" .= show arg ]
    , "message" .= ("Internal error. Partial counter grammar" :: TS.Text)
    ]
  toJSON (CounterArgumentError arg)
    = object
    [ "error" .= object
      [ "argument" .= show arg ]
    , "message" .= ("Invalid counter argument literal" :: TS.Text)
    ]
  toJSON (CounterArgumentParseError parseE arg)
    = object
    [ "error" .= object
      [ "parseError" .= show parseE
      , "argument" .= show arg
      ]
    , "message" .= ("Failed to parse counter argument" :: TS.Text)
    ]
  toJSON (EvaluationError expr typ)
    = object
    [ "error" .= object
      [ "expression" .= show expr
      , "type" .= show typ
      ]
    , "message" .= ("Expression cannot be evaluated due to bad final type. Should be TT" :: TS.Text)
    ]
  toJSON (ErrorCollection errs)
    = toJSON errs

resolved
  :: Unresolved a
  -> a
resolved (Unresolved a) = a

unresolved
  :: a
  -> Unresolved a
unresolved = Unresolved
