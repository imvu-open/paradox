{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Paradox.Eval.Aliases where

import Language.Paradox.Eval.Types
import Language.Haskell.Extract                 (functionExtractor)

import qualified Data.Text                      as TS
import qualified Data.Map.Strict                as M

allAliases :: M.Map TS.Text TypE
allAliases = M.fromList allAliases'

allAliases' :: [(TS.Text, TypE)]
allAliases' = $(functionExtractor "^alias[A-Z]")


aliasDyColor :: TypE
aliasDyColor =
    TypE (TS :~> TT :~> TT) $
        Apply GraphOptS (CounterN $ TS.pack "color")
aliasDyColorSaturation :: TypE
aliasDyColorSaturation =
    TypE (TD :~> TT :~> TT) $
        Apply GraphOptD (CounterN $ TS.pack "colorSaturation")
aliasDyColorValue :: TypE
aliasDyColorValue =
    TypE (TD :~> TT :~> TT) $
        Apply GraphOptD (CounterN $ TS.pack "colorValue")
aliasDyStrokeBorderColor :: TypE
aliasDyStrokeBorderColor =
    TypE (TS :~> TT :~> TT) $
        Apply GraphOptS (CounterN $ TS.pack "strokeBorderColor")
aliasDyStrokeWidth :: TypE
aliasDyStrokeWidth =
    TypE (TD :~> TT :~> TT) $
        Apply GraphOptD (CounterN $ TS.pack "strokeWidth")
aliasDyStrokeBorderWidth :: TypE
aliasDyStrokeBorderWidth =
    TypE (TD :~> TT :~> TT) $
        Apply GraphOptD (CounterN $ TS.pack "strokeBorderWidth")
aliasDyStrokePattern :: TypE
aliasDyStrokePattern =
    TypE (TIA :~> TT :~> TT) $
        Apply GraphOptIA (CounterN $ TS.pack "strokePattern")
aliasDyPointSize :: TypE
aliasDyPointSize =
    TypE (TI :~> TT :~> TT) $
        Apply GraphOptI (CounterN $ TS.pack "pointSize")
aliasDyFillGraph :: TypE
aliasDyFillGraph =
    TypE (TB :~> TT :~> TT) $
        Apply GraphOptB (CounterN $ TS.pack "fillGraph")
aliasDyFillAlpha :: TypE
aliasDyFillAlpha =
    TypE (TD :~> TT :~> TT) $
        Apply GraphOptD (CounterN $ TS.pack "fillAlpha")
aliasDyAxis :: TypE
aliasDyAxis =
    TypE (TS :~> TT :~> TT) $
        Apply GraphOptS (CounterN $ TS.pack "axis")
aliasDyDrawPoints :: TypE
aliasDyDrawPoints =
    TypE (TB :~> TT :~> TT) $
        Apply GraphOptB (CounterN $ TS.pack "drawPoints")
aliasDyConnectSeparatedPoints :: TypE
aliasDyConnectSeparatedPoints =
    TypE (TB :~> TT :~> TT) $
        Apply GraphOptB (CounterN $ TS.pack "connectSeparatedPoints")
aliasDyShowAnnotations :: TypE
aliasDyShowAnnotations =
    TypE (TB :~> TT :~> TT) $
        Apply GraphOptB (CounterN $ TS.pack "showAnnotations")
aliasDyDrawGapEdgePoints :: TypE
aliasDyDrawGapEdgePoints =
    TypE (TB :~> TT :~> TT) $
        Apply GraphOptB (CounterN $ TS.pack "drawGapEdgePoints")
aliasDyStackedGraph :: TypE
aliasDyStackedGraph =
    TypE (TB :~> TT :~> TT) $
        Apply SurfaceOptB (CounterN $ TS.pack "stackedGraph")
aliasDyStackedGraphNaNFill :: TypE
aliasDyStackedGraphNaNFill =
    TypE (TS :~> TT :~> TT) $
        Apply SurfaceOptS (CounterN $ TS.pack "stackedGraphNaNFill")
aliasDyAxisLogScaleY :: TypE
aliasDyAxisLogScaleY =
    TypE (TB :~> TT :~> TT) $
        Apply (Apply AxisOptB (CounterN $ TS.pack "y")) (CounterN $ TS.pack "logscale")
aliasDyAxisLogScaleY2 :: TypE
aliasDyAxisLogScaleY2 =
    TypE (TB :~> TT :~> TT) $
        Apply (Apply AxisOptB (CounterN $ TS.pack "y2")) (CounterN $ TS.pack "logscale")
aliasDyAxisIndependentTicksY :: TypE
aliasDyAxisIndependentTicksY =
    TypE (TB :~> TT :~> TT) $
        Apply (Apply AxisOptB (CounterN $ TS.pack "y")) (CounterN $ TS.pack "independentTicks")
aliasDyAxisIndependentTicksY2 :: TypE
aliasDyAxisIndependentTicksY2 =
    TypE (TB :~> TT :~> TT) $
        Apply (Apply AxisOptB (CounterN $ TS.pack "y2")) (CounterN $ TS.pack "independentTicks")
aliasDyAxisDrawAxisY :: TypE
aliasDyAxisDrawAxisY =
    TypE (TB :~> TT :~> TT) $
        Apply (Apply AxisOptB (CounterN $ TS.pack "y")) (CounterN $ TS.pack "drawAxis")
aliasDyAxisDrawAxisY2 :: TypE
aliasDyAxisDrawAxisY2 =
    TypE (TB :~> TT :~> TT) $
        Apply (Apply AxisOptB (CounterN $ TS.pack "y2")) (CounterN $ TS.pack "drawAxis")
aliasDyAxisIncludeZeroY :: TypE
aliasDyAxisIncludeZeroY =
    TypE (TB :~> TT :~> TT) $
        Apply (Apply AxisOptB (CounterN $ TS.pack "y")) (CounterN $ TS.pack "includeZero")
aliasDyAxisIncludeZeroY2 :: TypE
aliasDyAxisIncludeZeroY2 =
    TypE (TB :~> TT :~> TT) $
        Apply (Apply AxisOptB (CounterN $ TS.pack "y2")) (CounterN $ TS.pack "includeZero")
aliasDyGraphNulls :: TypE
aliasDyGraphNulls =
    TypE (TB :~> TT :~> TT) $
        Apply GraphOptB (CounterN $ TS.pack "graphNulls")
aliasDyStepPlot :: TypE
aliasDyStepPlot =
    TypE (TB :~> TT :~> TT) $
        Apply GraphOptB (CounterN $ TS.pack "stepPlot")
aliasDyForceStep :: TypE
aliasDyForceStep =
    TypE (TT :~> TT) $
        Apply
            (Apply GraphOptB (CounterN $ TS.pack "connectSeparatedPoints"))
            (BoolC True)
        :.:
        Apply
            (Apply GraphOptB (CounterN $ TS.pack "graphNulls"))
            (BoolC True)
        :.:
        Apply
            (Apply GraphOptB (CounterN $ TS.pack "stepPlot"))
            (BoolC True)
aliasDyStepNulls :: TypE
aliasDyStepNulls =
    TypE (TB :~> TT :~> TT) $
        Apply GraphOptB (CounterN $ TS.pack "connectSeparatedPoints")
        :>>>:
        Apply GraphOptB (CounterN $ TS.pack "graphNulls")
        :>>>:
        Apply GraphOptB (CounterN $ TS.pack "stepPlot")
aliasDyConstrainMin :: TypE
aliasDyConstrainMin =
    TypE (TD :~> TT :~> TT) $
        Apply SurfaceOptD (CounterN $ TS.pack "constrainMin")
aliasDyConstrainMax :: TypE
aliasDyConstrainMax =
    TypE (TD :~> TT :~> TT) $
        Apply SurfaceOptD (CounterN $ TS.pack "constrainMax")
aliasDyConstrainMinY2 :: TypE
aliasDyConstrainMinY2 =
    TypE (TD :~> TT :~> TT) $
        Apply SurfaceOptD (CounterN $ TS.pack "constrainMinY2")
aliasDyConstrainMaxY2 :: TypE
aliasDyConstrainMaxY2 =
    TypE (TD :~> TT :~> TT) $
        Apply SurfaceOptD (CounterN $ TS.pack "constrainMaxY2")
aliasDyConstrain :: TypE
aliasDyConstrain =
    TypE (TD :~> TD :~> TT :~> TT) $
        Apply SurfaceOptD (CounterN $ TS.pack "constrainMin")
        :...:
        Apply SurfaceOptD (CounterN $ TS.pack "constrainMax")
aliasDyConstrainY2 :: TypE
aliasDyConstrainY2 =
    TypE (TD :~> TD :~> TT :~> TT) $
        Apply SurfaceOptD (CounterN $ TS.pack "constrainMinY2")
        :...:
        Apply SurfaceOptD (CounterN $ TS.pack "constrainMaxY2")
aliasDyRemoveMinConstraint :: TypE
aliasDyRemoveMinConstraint =
    TypE (TB :~> TT :~> TT) $
        Apply SurfaceOptB (CounterN $ TS.pack "removeMinConstraint")
aliasDyRemoveMaxConstraint :: TypE
aliasDyRemoveMaxConstraint =
    TypE (TB :~> TT :~> TT) $
        Apply SurfaceOptB (CounterN $ TS.pack "removeMaxConstraint")
aliasDyRemoveMinConstraintY2 :: TypE
aliasDyRemoveMinConstraintY2 =
    TypE (TB :~> TT :~> TT) $
        Apply SurfaceOptB (CounterN $ TS.pack "removeMinConstraintY2")
aliasDyRemoveMaxConstraintY2 :: TypE
aliasDyRemoveMaxConstraintY2 =
    TypE (TB :~> TT :~> TT) $
        Apply SurfaceOptB (CounterN $ TS.pack "removeMaxConstraintY2")
aliasDyRemoveConstraint :: TypE
aliasDyRemoveConstraint =
    TypE (TB :~> TB :~> TT :~> TT) $
        Apply SurfaceOptB (CounterN $ TS.pack "removeMinConstraint")
        :...:
        Apply SurfaceOptB (CounterN $ TS.pack "removeMaxConstraint")
aliasDyRemoveConstraintY2 :: TypE
aliasDyRemoveConstraintY2 =
    TypE (TB :~> TB :~> TT :~> TT) $
        Apply SurfaceOptB (CounterN $ TS.pack "removeMinConstraintY2")
        :...:
        Apply SurfaceOptB (CounterN $ TS.pack "removeMaxConstraintY2")
