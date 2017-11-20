{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module Language.Paradox.Eval.Help (
  help
, helpToType
) where

import Language.Paradox.Eval.Types
import Language.Paradox.Eval.Symbols
import Language.Paradox.Eval.Aliases

import Language.Haskell.Extract (functionExtractor)

import Data.Aeson                                   ( (.=)
                                                    , toJSON
                                                    , ToJSON
                                                    , object
                                                    )

import qualified Data.Text as TS
import qualified Data.Map.Strict as M
import qualified Data.List as L

data WithHelp = forall a. WithHelp TS.Text (Typ a) (Maybe [TS.Text])

instance ToJSON WithHelp where
    toJSON (WithHelp t m ma) = object $ [
              "description" .= t
            , "type" .= helpToType m
        ] ++ aliases
        where
            aliases = case ma of
                Just a -> ["aliases" .= a]
                Nothing -> []

data TypeHelp = Part TS.Text | TagPart TypeHelp TypeHelp | FuncPart TypeHelp TypeHelp | FnPart TypeHelp TypeHelp

instance ToJSON TypeHelp where
    toJSON = go
        where
            go (Part t) = object [
                      "rep" .= t
                    , "t" .= ("text" :: TS.Text)
                ]
            go (TagPart l r) = object [
                      "sep" .= (", " :: TS.Text)
                    , "outerL" .= ("(" :: TS.Text)
                    , "outerR" .= (")" :: TS.Text)
                    , "left" .= toJSON l
                    , "right" .= toJSON r
                    , "t" .= ("tag" :: TS.Text)
                ]
            go (FuncPart l r) = object [
                      "sep" .= (" -> " :: TS.Text)
                    , "left" .= toJSON l
                    , "leftOuterL" .= ("(" :: TS.Text)
                    , "leftOuterR" .= (")" :: TS.Text)
                    , "right" .= toJSON r
                    , "t" .= ("func" :: TS.Text)
                ]
            go (FnPart l r) = object [
                      "sep" .= (" -> " :: TS.Text)
                    , "left" .= toJSON l
                    , "right" .= toJSON r
                    , "t" .= ("fn" :: TS.Text)
                ]

help :: M.Map TS.Text WithHelp
help = resolveAliases $ M.fromList helpfns

helpFns' :: [(TS.Text, (TS.Text, WithHelp))]
helpFns' = $(functionExtractor "^symbolHelp")

helpFnsAliases' :: [(TS.Text, (TS.Text, WithHelp))]
helpFnsAliases' = $(functionExtractor "^aliasHelp")

helpfns :: [(TS.Text, WithHelp)]
helpfns = map snd helpFns' ++ map snd helpFnsAliases'

resolveAliases :: M.Map TS.Text WithHelp
               -> M.Map TS.Text WithHelp
resolveAliases orig =
    let aliasPass = M.foldrWithKey m M.empty orig
        m k (WithHelp desc typ aliasesm) m' = case aliasesm of
            Just aliases ->
                foldr (\al m'' -> let newAliases = k : L.delete al aliases
                       in M.insert al (WithHelp desc typ (Just newAliases)) m'') m' aliases
            Nothing -> m'
    in M.union orig aliasPass

helpToType :: Typ a
           -> TypeHelp
helpToType TT = Part "ReturnData"
helpToType TS = Part "String"
helpToType TD = Part "Double"
helpToType TB = Part "Bool"
helpToType TI = Part "Integer"
helpToType TIA = Part "[Integer]"
helpToType TDA = Part "[Double]"
helpToType TSA = Part "[String]"
helpToType TBA = Part "[Bool]"
helpToType TTSC = Part "TimeSeriesChunk"
helpToType (TTag l r) = TagPart (helpToType l) (helpToType r)
helpToType (a@(_ :~> _) :~> b) = FuncPart (helpToType a) (helpToType b)
helpToType (a :~> b) = FnPart (helpToType a) (helpToType b)


generalHelp :: TypE -> TS.Text -> TS.Text -> Maybe [TS.Text] -> (TS.Text, WithHelp)
generalHelp typE name msg aliases =
    case typE of
      TypE typ _ -> ( name
                    , WithHelp
                        msg
                        typ
                        aliases
                    )

symbolHelpSum :: (TS.Text, WithHelp)
symbolHelpSum =
    generalHelp symbolSum "sum" "Adds together multiple timeseries" (Just ["add"])

symbolHelpMaxMaxAtEachSample :: (TS.Text, WithHelp)
symbolHelpMaxMaxAtEachSample =
    generalHelp symbolMaxMaxAtEachSample "maxMaxAtEachSample" "For a given list of timeseries get the maximum max value at each x value" Nothing

symbolHelpMaxAvgAtEachSample :: (TS.Text, WithHelp)
symbolHelpMaxAvgAtEachSample =
    generalHelp symbolMaxAvgAtEachSample "maxAvgAtEachSample" "For a given list of timeseries get the maximum avg value at each x value" Nothing

symbolHelpMaxMinAtEachSample :: (TS.Text, WithHelp)
symbolHelpMaxMinAtEachSample =
    generalHelp symbolMaxMinAtEachSample "maxMinAtEachSample" "For a given list of timeseries get the maximum min value at each x value" Nothing

symbolHelpMaxValueAtEachSample :: (TS.Text, WithHelp)
symbolHelpMaxValueAtEachSample =
    generalHelp symbolMaxValueAtEachSample "maxValueAtEachSample" "For a given list of timeseries get the maximum value at each x value, using the passed in field accessor" Nothing
symbolHelpGraphMax :: (TS.Text, WithHelp)
symbolHelpGraphMax =
    generalHelp symbolGraphMax "graphMax" "Changes the value graphed to the max at each bucket" Nothing

symbolHelpGraphCount :: (TS.Text, WithHelp)
symbolHelpGraphCount =
    generalHelp symbolGraphCount "graphCount" "Changes the value graphed to the count at each bucket" Nothing

symbolHelpGraphMin :: (TS.Text, WithHelp)
symbolHelpGraphMin =
    generalHelp symbolGraphMin "graphMin" "Changes the value graphed to the min at each bucket" Nothing

symbolHelpGraphAvg :: (TS.Text, WithHelp)
symbolHelpGraphAvg =
    generalHelp symbolGraphAvg "graphAvg" "Changes the value graphed to the avg at each bucket" Nothing

symbolHelpGraphField :: (TS.Text, WithHelp)
symbolHelpGraphField =
    generalHelp symbolGraphField "graphField" "Changes the value graphed to the one specified by the field accessor, at each bucket" Nothing

symbolHelpSummarizeAvg :: (TS.Text, WithHelp)
symbolHelpSummarizeAvg =
    generalHelp symbolSummarizeAvg "summarizeAvg" "Summarizes using an average of buckets for the given interval" Nothing

symbolHelpSummarizeSum :: (TS.Text, WithHelp)
symbolHelpSummarizeSum =
    generalHelp symbolSummarizeSum "summarizeSum" "Summarizes using a sum of buckets for the given interval" Nothing

symbolHelpAvg :: (TS.Text, WithHelp)
symbolHelpAvg =
    generalHelp symbolAvg "avg" "Averages a list of timeseries" (Just ["average"])

symbolHelpAbs :: (TS.Text, WithHelp)
symbolHelpAbs =
    generalHelp symbolAbs "abs" "For each value in all time series, get the absolute value" (Just ["absoluteValue"])

symbolHelpMovingAverage :: (TS.Text, WithHelp)
symbolHelpMovingAverage =
    generalHelp symbolMovingAverage "movingAverage" "Calculate the moving average of each timeseries" Nothing

symbolHelpMedianFilter :: (TS.Text, WithHelp)
symbolHelpMedianFilter =
    generalHelp symbolMedianFilter "medianFilter" "Calculate a median filter of each timeseries" Nothing

symbolHelpMedianAbsoluteDeviationFilter :: (TS.Text, WithHelp)
symbolHelpMedianAbsoluteDeviationFilter =
    generalHelp symbolMedianAbsoluteDeviationFilter "medianAbsoluteDeviationFilter" "Filter out any points that do not fall withing a median absolute deviance of the passed in value" Nothing

symbolHelpMedianAbsoluteDeviation :: (TS.Text, WithHelp)
symbolHelpMedianAbsoluteDeviation =
    generalHelp symbolMedianAbsoluteDeviation "medianAbsoluteDeviation" "The median absolute deviance of the values in the series list" Nothing

symbolHelpDoubleMedianAbsoluteDeviationFilter :: (TS.Text, WithHelp)
symbolHelpDoubleMedianAbsoluteDeviationFilter =
    generalHelp symbolDoubleMedianAbsoluteDeviationFilter "doubleMedianAbsoluteDeviationFilter" "Filter out any points that do not fall withing a median absolute deviance of the passed in value. Calculates mad for < med and > med seperately" Nothing

symbolHelpDoubleMedianAbsoluteDeviation :: (TS.Text, WithHelp)
symbolHelpDoubleMedianAbsoluteDeviation =
    generalHelp symbolDoubleMedianAbsoluteDeviation "doubleMedianAbsoluteDeviation" "The median absolute deviance of the values in the series list. Calculates mad for < med and > med seperately" Nothing

symbolHelpLoess :: (TS.Text, WithHelp)
symbolHelpLoess =
    generalHelp symbolLoess "loess" "Calculates loess on graph" Nothing

symbolHelpEDMMulti :: (TS.Text, WithHelp)
symbolHelpEDMMulti =
    generalHelp symbolEDMMulti "edmMulti" "Calculates edmMulti on graph" Nothing

symbolHelpStl :: (TS.Text, WithHelp)
symbolHelpStl =
    generalHelp symbolStl "stl" "Calculates stl on graph and displays all the components" Nothing

symbolHelpStlSeason :: (TS.Text, WithHelp)
symbolHelpStlSeason =
    generalHelp symbolStlSeason "stlSeason" "Calculates stl on graph and displays the seasonal component" Nothing

symbolHelpStlTrend :: (TS.Text, WithHelp)
symbolHelpStlTrend =
    generalHelp symbolStlTrend "stlTrend" "Calculates stl on graph and displays the trend coponent" Nothing

symbolHelpStlResidual :: (TS.Text, WithHelp)
symbolHelpStlResidual =
    generalHelp symbolStlResidual "stlResidual" "Calculates stl on graph and displays the residuals" Nothing

symbolHelpOffset :: (TS.Text, WithHelp)
symbolHelpOffset =
    generalHelp symbolOffset "offset" "offset every value in each timeseries by the given value" Nothing

symbolHelpKeepSeriesAboveDeviation :: (TS.Text, WithHelp)
symbolHelpKeepSeriesAboveDeviation =
    generalHelp symbolKeepSeriesAboveDeviation "keepSeriesAboveDeviation" "Given a standard deviation, will keep only those series with at least one value above outside that number of deviations" Nothing

symbolHelpKeepSeriesAboveDeviationInc :: (TS.Text, WithHelp)
symbolHelpKeepSeriesAboveDeviationInc =
    generalHelp symbolKeepSeriesAboveDeviationInc "keepSeriesAboveDeviationInc" "Given a standard deviation, will keep only those series with at least one value above outside that number of deviations" Nothing

symbolHelpKeepSeriesWithinDeviation :: (TS.Text, WithHelp)
symbolHelpKeepSeriesWithinDeviation =
    generalHelp symbolKeepSeriesWithinDeviation "keepSeriesWithinDeviation" "Given a standard deviation, will keep only those series with all values inside that number of deviations" Nothing

symbolHelpKeepSeriesWithinDeviationInc :: (TS.Text, WithHelp)
symbolHelpKeepSeriesWithinDeviationInc =
    generalHelp symbolKeepSeriesWithinDeviationInc "keepSeriesWithinDeviationInc" "Given a standard deviation, will keep only those series with all values inside that number of deviations" Nothing

symbolHelpRemoveAbovePercentile :: (TS.Text, WithHelp)
symbolHelpRemoveAbovePercentile =
    generalHelp symbolRemoveAbovePercentile "removeAbovePercentile" "This function will remove values that are above the percentile passed in the first argument." Nothing

symbolHelpRemoveAbovePercentileInc :: (TS.Text, WithHelp)
symbolHelpRemoveAbovePercentileInc =
    generalHelp symbolRemoveAbovePercentileInc "removeAbovePercentileInc" "This function will remove values that are equalt to or above the percentile passed in the first argument." Nothing

symbolHelpRemoveBelowPercentile :: (TS.Text, WithHelp)
symbolHelpRemoveBelowPercentile =
    generalHelp symbolRemoveBelowPercentile "removeBelowPercentile" "This function will remove values that are below the percentile passed in the first argument." Nothing

symbolHelpRemoveBelowPercentileInc :: (TS.Text, WithHelp)
symbolHelpRemoveBelowPercentileInc =
    generalHelp symbolRemoveBelowPercentileInc "removeBelowPercentileInc" "This function will remove values that are equal to or below the percentile passed in the first argument." Nothing

symbolHelpRemoveAboveValue :: (TS.Text, WithHelp)
symbolHelpRemoveAboveValue =
    generalHelp symbolRemoveAboveValue "removeAboveValue" "This function will remove values that are above the value passed in the first argument." Nothing

symbolHelpRemoveAboveValueInc :: (TS.Text, WithHelp)
symbolHelpRemoveAboveValueInc =
    generalHelp symbolRemoveAboveValueInc "removeAboveValueInc" "This function will remove values that are equal to or above the value passed in the first argument." Nothing

symbolHelpRemoveBelowValue :: (TS.Text, WithHelp)
symbolHelpRemoveBelowValue =
    generalHelp symbolRemoveBelowValue "removeBelowValue" "This function will remove values that are below the value passed in the first argument." Nothing

symbolHelpRemoveBelowValueInc :: (TS.Text, WithHelp)
symbolHelpRemoveBelowValueInc =
    generalHelp symbolRemoveBelowValueInc "removeBelowValueInc" "This function will remove values that are equal to or below the value passed in the first argument." Nothing

symbolHelpCeiling :: (TS.Text, WithHelp)
symbolHelpCeiling =
    generalHelp symbolCeiling "ceiling" "This function will cap values that are above the value passed in the first argument." Nothing

symbolHelpFloor :: (TS.Text, WithHelp)
symbolHelpFloor =
    generalHelp symbolFloor "floor" "This function will cap values that are below the value passed in the first argument." Nothing

symbolHelpKeepSeriesAbove :: (TS.Text, WithHelp)
symbolHelpKeepSeriesAbove =
    generalHelp symbolKeepSeriesAbove "keepSeriesAbove" "This function will take a list of series and only return the ones who have at least one point above the value passed in the first argument." Nothing

symbolHelpKeepSeriesAboveInc :: (TS.Text, WithHelp)
symbolHelpKeepSeriesAboveInc =
    generalHelp symbolKeepSeriesAboveInc "keepSeriesAboveInc" "This function will take a list of series and only return the ones who have at least one point equal to or above the value passed in the first argument." Nothing

symbolHelpKeepSeriesBelow :: (TS.Text, WithHelp)
symbolHelpKeepSeriesBelow =
    generalHelp symbolKeepSeriesBelow "keepSeriesBelow" "This function will take a list of series and only return the ones who have at least one point below the value passed in the first argument." Nothing

symbolHelpKeepSeriesBelowInc :: (TS.Text, WithHelp)
symbolHelpKeepSeriesBelowInc =
    generalHelp symbolKeepSeriesBelowInc "keepSeriesBelowInc" "This function will take a list of series and only return the ones who have at least one point equal to or below the value passed in the first argument." Nothing

symbolHelpKeepSeriesAllAbove :: (TS.Text, WithHelp)
symbolHelpKeepSeriesAllAbove =
    generalHelp symbolKeepSeriesAllAbove "keepSeriesAllAbove" "This function will take a list of series and only return the ones who have all points above the value passed in the first argument." Nothing

symbolHelpKeepSeriesAllAboveInc :: (TS.Text, WithHelp)
symbolHelpKeepSeriesAllAboveInc =
    generalHelp symbolKeepSeriesAllAboveInc "keepSeriesAllAboveInc" "This function will take a list of series and only return the ones who have all points equal to or above the value passed in the first argument." Nothing

symbolHelpKeepSeriesAllBelow :: (TS.Text, WithHelp)
symbolHelpKeepSeriesAllBelow =
    generalHelp symbolKeepSeriesAllBelow "keepSeriesAllBelow" "This function will take a list of series and only return the ones who have all points below the value passed in the first argument." Nothing

symbolHelpKeepSeriesAllBelowInc :: (TS.Text, WithHelp)
symbolHelpKeepSeriesAllBelowInc =
    generalHelp symbolKeepSeriesAllBelowInc "keepSeriesAllBelowInc" "This function will take a list of series and only return the ones who have all points equal to or below the value passed in the first argument." Nothing

symbolHelpAlias :: (TS.Text, WithHelp)
symbolHelpAlias =
    generalHelp symbolAlias "alias" "Renames a series list to the string passed in" Nothing

symbolHelpSmartAlias :: (TS.Text, WithHelp)
symbolHelpSmartAlias =
    generalHelp symbolSmartAlias "smartAlias" "Renames a series list. Will attempt to select the counter part of a processed series list and split it on . and then replace all instances of {} in the string parameter with the counter part at the int index (0 indexed)" Nothing

symbolHelpSmartAliasA :: (TS.Text, WithHelp)
symbolHelpSmartAliasA =
    generalHelp symbolSmartAliasA "smartAliasA" "Renames a series list. Will attempt to select the counter parts of a processed series list and split it on . and then replace all instances of {#n} in the string parameter with the counter part at the int index (0 indexed)" Nothing

symbolHelpExclude :: (TS.Text, WithHelp)
symbolHelpExclude =
    generalHelp symbolExclude "exclude" "Removes all counters from the series list whose name matches the regex. Will also prevent needless fetching of counters from backend" Nothing

symbolHelpInclude :: (TS.Text, WithHelp)
symbolHelpInclude =
    generalHelp symbolInclude "include" "Only include counters from the series list whose name matches the regex. Will also prevent needless fetching of counters from backend" Nothing

symbolHelpScale :: (TS.Text, WithHelp)
symbolHelpScale =
    generalHelp symbolScale "scale" "Scale each value in all series in the series list by the value" Nothing

symbolHelpScalarDivide :: (TS.Text, WithHelp)
symbolHelpScalarDivide =
    generalHelp symbolScalarDivide "scalarDivide" "divide each value in all series in the series list by the value" Nothing

symbolHelpScaleByInterval :: (TS.Text, WithHelp)
symbolHelpScaleByInterval =
    generalHelp symbolScaleByInterval "scaleByInterval" "Scale each value in the series in the series list by the interval the backend returns. Useful with counter/event timeseries" Nothing

symbolHelpDivideByInterval :: (TS.Text, WithHelp)
symbolHelpDivideByInterval =
    generalHelp symbolDivideByInterval "divideByInterval" "Modify each value in the series in the series list by dividing it by the interval the backend returns." Nothing

symbolHelpIntegrate :: (TS.Text, WithHelp)
symbolHelpIntegrate =
    generalHelp symbolIntegrate "integrate" "Produce a running sum over time of each time series in the series list" Nothing

symbolHelpDerive :: (TS.Text, WithHelp)
symbolHelpDerive =
    generalHelp symbolDerive "derive" "Calculate the change between each data point in each of the timeseries in the series list" Nothing

symbolHelpLog :: (TS.Text, WithHelp)
symbolHelpLog =
    generalHelp symbolLog "log" "Logarithmic representation of each data point in each of the timeseries in the series list. Arguments are integer base, double offset, integer bias. Applied as <log_{base}({data_point} + {offset}) + {bias}>" Nothing

symbolHelpSub :: (TS.Text, WithHelp)
symbolHelpSub =
    generalHelp symbolSub "sub" "subtract the second time series list from the first timeseries. First time series list should be reduced. Will sum the second time series list" (Just ["subtract"])

symbolHelpDivide :: (TS.Text, WithHelp)
symbolHelpDivide =
    generalHelp symbolDivide "divide" "divide the first timeseries by the second time series list. First time series list should be reduced. Will sum the second time series list" Nothing

symbolHelpCons :: (TS.Text, WithHelp)
symbolHelpCons =
    generalHelp symbolCons "cons" "combines 2 time series lists into a new timeseries list" Nothing

symbolHelpConstantLine :: (TS.Text, WithHelp)
symbolHelpConstantLine =
    generalHelp symbolConstantLine "constantLine" "Generate a time series list with the argument value at all points" Nothing

symbolHelpGraphOptions :: (TS.Text, WithHelp)
symbolHelpGraphOptions =
    generalHelp symbolGraphOptions "graphOptions" "set per graph options for dyGraph front end. Takes a json string" Nothing


symbolHelpGraphOptionI :: (TS.Text, WithHelp)
symbolHelpGraphOptionI =
    generalHelp symbolGraphOptionI "graphOptionI" "set per graph options for dyGraph front end. Takes an option name and a typed value. Integer variant" Nothing

symbolHelpGraphOptionD :: (TS.Text, WithHelp)
symbolHelpGraphOptionD =
    generalHelp symbolGraphOptionD "graphOptionD" "set per graph options for dyGraph front end. Takes an option name and a typed value. Double variant" Nothing

symbolHelpGraphOptionS :: (TS.Text, WithHelp)
symbolHelpGraphOptionS =
    generalHelp symbolGraphOptionS "graphOptionS" "set per graph options for dyGraph front end. Takes an option name and a typed value. String variant" Nothing

symbolHelpGraphOptionB :: (TS.Text, WithHelp)
symbolHelpGraphOptionB =
    generalHelp symbolGraphOptionB "graphOptionB" "set per graph options for dyGraph front end. Takes an option name and a typed value. Bool variant" Nothing

symbolHelpGraphOptionIA :: (TS.Text, WithHelp)
symbolHelpGraphOptionIA =
    generalHelp symbolGraphOptionIA "graphOptionIA" "set per graph options for dyGraph front end. Takes an option name and a typed value. Integer Array variant" Nothing

symbolHelpGraphOptionDA :: (TS.Text, WithHelp)
symbolHelpGraphOptionDA =
    generalHelp symbolGraphOptionDA "graphOptionDA" "set per graph options for dyGraph front end. Takes an option name and a typed value. Double Array variant" Nothing

symbolHelpGraphOptionSA :: (TS.Text, WithHelp)
symbolHelpGraphOptionSA =
    generalHelp symbolGraphOptionSA "graphOptionSA" "set per graph options for dyGraph front end. Takes an option name and a typed value. String Array variant" Nothing

symbolHelpGraphOptionBA :: (TS.Text, WithHelp)
symbolHelpGraphOptionBA =
    generalHelp symbolGraphOptionBA "graphOptionBA" "set per graph options for dyGraph front end. Takes an option name and a typed value. Bool Array variant" Nothing


symbolHelpAxisOptions :: (TS.Text, WithHelp)
symbolHelpAxisOptions =
    generalHelp symbolAxisOptions "axisOptions" "set per axis options for dyGraph front end. Takes a json string" Nothing


symbolHelpAxisOptionI :: (TS.Text, WithHelp)
symbolHelpAxisOptionI =
    generalHelp symbolAxisOptionI "axisOptionI" "set per axis options for dyGraph front end. Takes an option name and a typed value. Integer variant" Nothing

symbolHelpAxisOptionD :: (TS.Text, WithHelp)
symbolHelpAxisOptionD =
    generalHelp symbolAxisOptionD "axisOptionD" "set per axis options for dyGraph front end. Takes an option name and a typed value. Double variant" Nothing

symbolHelpAxisOptionS :: (TS.Text, WithHelp)
symbolHelpAxisOptionS =
    generalHelp symbolAxisOptionS "axisOptionS" "set per axis options for dyGraph front end. Takes an option name and a typed value. String variant" Nothing

symbolHelpAxisOptionB :: (TS.Text, WithHelp)
symbolHelpAxisOptionB =
    generalHelp symbolAxisOptionB "axisOptionB" "set per axis options for dyGraph front end. Takes an option name and a typed value. Bool variant" Nothing

symbolHelpAxisOptionIA :: (TS.Text, WithHelp)
symbolHelpAxisOptionIA =
    generalHelp symbolAxisOptionIA "axisOptionIA" "set per axis options for dyGraph front end. Takes an option name and a typed value. Integer Array variant" Nothing

symbolHelpAxisOptionDA :: (TS.Text, WithHelp)
symbolHelpAxisOptionDA =
    generalHelp symbolAxisOptionDA "axisOptionDA" "set per axis options for dyGraph front end. Takes an option name and a typed value. Double Array variant" Nothing

symbolHelpAxisOptionSA :: (TS.Text, WithHelp)
symbolHelpAxisOptionSA =
    generalHelp symbolAxisOptionSA "axisOptionSA" "set per axis options for dyGraph front end. Takes an option name and a typed value. String Array variant" Nothing

symbolHelpAxisOptionBA :: (TS.Text, WithHelp)
symbolHelpAxisOptionBA =
    generalHelp symbolAxisOptionBA "axisOptionBA" "set per axis options for dyGraph front end. Takes an option name and a typed value. Bool Array variant" Nothing


symbolHelpSurfaceOptions :: (TS.Text, WithHelp)
symbolHelpSurfaceOptions =
    generalHelp symbolSurfaceOptions "surfaceOptions" "set per surface options for dyGraph front end. Takes a json string" Nothing


symbolHelpSurfaceOption :: (TS.Text, WithHelp)
symbolHelpSurfaceOption =
    generalHelp symbolSurfaceOption "surfaceOption" "set per surface options for dyGraph front end. Takes an option name" Nothing

symbolHelpSurfaceOptionI :: (TS.Text, WithHelp)
symbolHelpSurfaceOptionI =
    generalHelp symbolSurfaceOptionI "surfaceOptionI" "set per surface options for dyGraph front end. Takes an option name and a typed value. Integer variant" Nothing

symbolHelpSurfaceOptionD :: (TS.Text, WithHelp)
symbolHelpSurfaceOptionD =
    generalHelp symbolSurfaceOptionD "surfaceOptionD" "set per surface options for dyGraph front end. Takes an option name and a typed value. Double variant" Nothing

symbolHelpSurfaceOptionS :: (TS.Text, WithHelp)
symbolHelpSurfaceOptionS =
    generalHelp symbolSurfaceOptionS "surfaceOptionS" "set per surface options for dyGraph front end. Takes an option name and a typed value. String variant" Nothing

symbolHelpSurfaceOptionB :: (TS.Text, WithHelp)
symbolHelpSurfaceOptionB =
    generalHelp symbolSurfaceOptionB "surfaceOptionB" "set per surface options for dyGraph front end. Takes an option name and a typed value. Bool variant" Nothing

symbolHelpSurfaceOptionIA :: (TS.Text, WithHelp)
symbolHelpSurfaceOptionIA =
    generalHelp symbolSurfaceOptionIA "surfaceOptionIA" "set per surface options for dyGraph front end. Takes an option name and a typed value. Integer Array variant" Nothing

symbolHelpSurfaceOptionDA :: (TS.Text, WithHelp)
symbolHelpSurfaceOptionDA =
    generalHelp symbolSurfaceOptionDA "surfaceOptionDA" "set per surface options for dyGraph front end. Takes an option name and a typed value. Double Array variant" Nothing

symbolHelpSurfaceOptionSA :: (TS.Text, WithHelp)
symbolHelpSurfaceOptionSA =
    generalHelp symbolSurfaceOptionSA "surfaceOptionSA" "set per surface options for dyGraph front end. Takes an option name and a typed value. String Array variant" Nothing

symbolHelpSurfaceOptionBA :: (TS.Text, WithHelp)
symbolHelpSurfaceOptionBA =
    generalHelp symbolSurfaceOptionBA "surfaceOptionBA" "set per surface options for dyGraph front end. Takes an option name and a typed value. Bool Array variant" Nothing


symbolHelpAddAxis :: (TS.Text, WithHelp)
symbolHelpAddAxis =
    generalHelp symbolAddAxis "addAxis" "Add a second axis to the frontend" Nothing

symbolHelpMap :: (TS.Text, WithHelp)
symbolHelpMap =
    generalHelp symbolMap "map" "Higher order application. Applies the action to each time series in the time series list." Nothing


symbolHelpWhen :: (TS.Text, WithHelp)
symbolHelpWhen =
    generalHelp symbolWhen "when" "Conditional. Applies the action when a predicate is true" Nothing

symbolHelpWhenElse :: (TS.Text, WithHelp)
symbolHelpWhenElse =
    generalHelp symbolWhenElse "whenElse" "Conditional. Applies an action when a predicate is true, and another when its false" Nothing


symbolHelpWhenTransformed :: (TS.Text, WithHelp)
symbolHelpWhenTransformed =
    generalHelp symbolWhenTransformed "whenTransformed" "Conditional. Applies the action when a predicate is true on data transformed by another action" Nothing

symbolHelpWhenElseTransformed :: (TS.Text, WithHelp)
symbolHelpWhenElseTransformed =
    generalHelp symbolWhenElseTransformed "whenElseTransformed" "Conditional. Applies the action when a predicate is true on data transformed by another action, applies another action on false" Nothing


symbolHelpHasAnyValue :: (TS.Text, WithHelp)
symbolHelpHasAnyValue =
    generalHelp symbolHasAnyValue "hasAnyValue" "Timeseries list Predicate function: checks that any value matches the double predicate " Nothing

symbolHelpHasAllValue :: (TS.Text, WithHelp)
symbolHelpHasAllValue =
    generalHelp symbolHasAllValue "hasAllValue" "Timeseries list Predicate function: checks that all value matches the double predicate " Nothing

symbolHelpHasNoValue :: (TS.Text, WithHelp)
symbolHelpHasNoValue =
    generalHelp symbolHasNoValue "hasNoValue" "Timeseries list Predicate function: checks that no value matches the double predicate " Nothing


symbolHelpHasAnyValueInRange :: (TS.Text, WithHelp)
symbolHelpHasAnyValueInRange =
    generalHelp symbolHasAnyValueInRange "hasAnyValueInRange" "Timeseries list Predicate function: checks that any value in the right biased percentile range matches the double predicate " Nothing

symbolHelpHasAllValueInRange :: (TS.Text, WithHelp)
symbolHelpHasAllValueInRange =
    generalHelp symbolHasAllValueInRange "hasAllValueInRange" "Timeseries list Predicate function: checks that all value in the right biased percentile range matches the double predicate " Nothing

symbolHelpHasNoValueInRange :: (TS.Text, WithHelp)
symbolHelpHasNoValueInRange =
    generalHelp symbolHasNoValueInRange "hasNoValueInRange" "Timeseries list Predicate function: checks that no value in the right biased percentile range matches the double predicate " Nothing


symbolHelpGreaterThan :: (TS.Text, WithHelp)
symbolHelpGreaterThan =
    generalHelp symbolGreaterThan "greaterThan" "Double Predicate function, >" Nothing

symbolHelpGreaterThanOrEqual :: (TS.Text, WithHelp)
symbolHelpGreaterThanOrEqual =
    generalHelp symbolGreaterThanOrEqual "greaterThanOrEqual" "Double Predicate function, >=" Nothing

symbolHelpLessThan :: (TS.Text, WithHelp)
symbolHelpLessThan =
    generalHelp symbolLessThan "lessThan" "Double Predicate function, <" Nothing

symbolHelpLessThanOrEqual :: (TS.Text, WithHelp)
symbolHelpLessThanOrEqual =
    generalHelp symbolLessThanOrEqual "lessThanOrEqual" "Double Predicate function, <=" Nothing


symbolHelpGetAvg :: (TS.Text, WithHelp)
symbolHelpGetAvg =
    generalHelp symbolGetAvg "getAvg" "Field accessor, gets the avg field" Nothing

symbolHelpGetMin :: (TS.Text, WithHelp)
symbolHelpGetMin =
    generalHelp symbolGetMin "getMin" "Field accessor, gets the min field" Nothing

symbolHelpGetMax :: (TS.Text, WithHelp)
symbolHelpGetMax =
    generalHelp symbolGetMax "getMax" "Field accessor, gets the max field" Nothing

symbolHelpGetCount :: (TS.Text, WithHelp)
symbolHelpGetCount =
    generalHelp symbolGetCount "getCount" "Field accessor, gets the count field" Nothing


symbolHelpNot :: (TS.Text, WithHelp)
symbolHelpNot =
    generalHelp symbolNot "not" "Inverts boolean operations" Nothing


symbolHelpApplyWithValue :: (TS.Text, WithHelp)
symbolHelpApplyWithValue =
    generalHelp symbolApplyWithValue "applyWithValue" "Applies the given function (that takes a double), using a double it gets with the second argument function" Nothing

symbolHelpGetLargest :: (TS.Text, WithHelp)
symbolHelpGetLargest =
    generalHelp symbolGetLargest "getLargest" "Select the largest value from the time series, using the filed selector" Nothing

symbolHelpGetSmallest :: (TS.Text, WithHelp)
symbolHelpGetSmallest =
    generalHelp symbolGetSmallest "getSmallest" "Select the largest value from the time series, using the filed selector" Nothing

symbolHelpGetLargestInRange :: (TS.Text, WithHelp)
symbolHelpGetLargestInRange =
    generalHelp symbolGetLargestInRange "getLargestInRange" "Given a percent range (right biased), select the largest value from the time series, using the filed selector" Nothing

symbolHelpGetSmallestInRange :: (TS.Text, WithHelp)
symbolHelpGetSmallestInRange =
    generalHelp symbolGetSmallestInRange "getSmallestInRange" "Given a percent range (right biased), select the smallest value from the time series, using the filed selector" Nothing


symbolHelpLargerD :: (TS.Text, WithHelp)
symbolHelpLargerD =
    generalHelp symbolLargerD "largerD" "Select the larger double" Nothing

symbolHelpSmallerD :: (TS.Text, WithHelp)
symbolHelpSmallerD =
    generalHelp symbolSmallerD "smallerD" "Select the smaller double" Nothing

symbolHelpMulD :: (TS.Text, WithHelp)
symbolHelpMulD =
    generalHelp symbolMulD "mulD" "Simple double multiplication" Nothing

symbolHelpAddD :: (TS.Text, WithHelp)
symbolHelpAddD =
    generalHelp symbolAddD "addD" "Simple double addition" Nothing

symbolHelpDivD :: (TS.Text, WithHelp)
symbolHelpDivD =
    generalHelp symbolDivD "divD" "Simple double division" Nothing

symbolHelpSubD :: (TS.Text, WithHelp)
symbolHelpSubD =
    generalHelp symbolSubD "subD" "Simple double subtraction" Nothing


aliasHelpDyColor :: (TS.Text, WithHelp)
aliasHelpDyColor =
    generalHelp aliasDyColor "dyColor" "Sets the query results color" Nothing
aliasHelpDyColorSaturation :: (TS.Text, WithHelp)
aliasHelpDyColorSaturation =
    generalHelp aliasDyColorSaturation "dyColorSaturation" "Sets the query results color saturation" Nothing
aliasHelpDyColorValue :: (TS.Text, WithHelp)
aliasHelpDyColorValue =
    generalHelp aliasDyColorValue "dyColorValue" "Sets the query results color value" Nothing
aliasHelpDyStrokeBorderColor :: (TS.Text, WithHelp)
aliasHelpDyStrokeBorderColor =
    generalHelp aliasDyStrokeBorderColor "dyStrokeBorderColor" "Sets the query results stroke border color" Nothing
aliasHelpDyStrokeWidth :: (TS.Text, WithHelp)
aliasHelpDyStrokeWidth =
    generalHelp aliasDyStrokeWidth "dyStrokeWidth" "Sets the query results stroke border width" Nothing
aliasHelpDyStrokeBorderWidth :: (TS.Text, WithHelp)
aliasHelpDyStrokeBorderWidth =
    generalHelp aliasDyStrokeBorderWidth "dyStrokeBorderWidth" "Sets the query results stroke border width" Nothing
aliasHelpDyStrokePattern :: (TS.Text, WithHelp)
aliasHelpDyStrokePattern =
    generalHelp aliasDyStrokePattern "dyStrokePattern" "Sets the query results stroke border pattern" Nothing
aliasHelpDyPointSize :: (TS.Text, WithHelp)
aliasHelpDyPointSize =
    generalHelp aliasDyPointSize "dyPointSize" "Sets the query results point size" Nothing
aliasHelpDyFillGraph :: (TS.Text, WithHelp)
aliasHelpDyFillGraph =
    generalHelp aliasDyFillGraph "dyFillGraph" "Sets whether the query results graphs should fill" Nothing
aliasHelpDyFillAlpha :: (TS.Text, WithHelp)
aliasHelpDyFillAlpha =
    generalHelp aliasDyFillAlpha "dyFillAlpha" "Sets the query results fill alpha" Nothing
aliasHelpDyAxis :: (TS.Text, WithHelp)
aliasHelpDyAxis =
    generalHelp aliasDyAxis "dyAxis" "Sets which axis the query results should graph on" Nothing
aliasHelpDyDrawPoints :: (TS.Text, WithHelp)
aliasHelpDyDrawPoints =
    generalHelp aliasDyDrawPoints "dyDrawPoints" "Sets whether the query results should draw points" Nothing
aliasHelpDyConnectSeparatedPoints :: (TS.Text, WithHelp)
aliasHelpDyConnectSeparatedPoints =
    generalHelp aliasDyConnectSeparatedPoints "dyConnectSeparatedPoints" "Sets whether the query results should draw separate points with lines connecting them" Nothing
aliasHelpDyShowAnnotations :: (TS.Text, WithHelp)
aliasHelpDyShowAnnotations =
    generalHelp aliasDyShowAnnotations "dyShowAnnotations" "Sets whether the query should draw min max annotations" Nothing
aliasHelpDyDrawGapEdgePoints :: (TS.Text, WithHelp)
aliasHelpDyDrawGapEdgePoints =
    generalHelp aliasDyDrawGapEdgePoints "dyDrawGapEdgePoints" "Sets whether the query results should fill in for edge gaps" Nothing
aliasHelpDyStackedGraph :: (TS.Text, WithHelp)
aliasHelpDyStackedGraph =
    generalHelp aliasDyStackedGraph "dyStackedGraph" "Sets whether the query results should be stacked" Nothing
aliasHelpDyStackedGraphNaNFill :: (TS.Text, WithHelp)
aliasHelpDyStackedGraphNaNFill =
    generalHelp aliasDyStackedGraphNaNFill "dyStackedGraphNaNFill" "Sets how the query results should handle NaN when stacked" Nothing
aliasHelpDyAxisLogScaleY :: (TS.Text, WithHelp)
aliasHelpDyAxisLogScaleY =
    generalHelp aliasDyAxisLogScaleY "dyAxisLogScaleY" "Toggles log scaling for Y1 axis for the query results" Nothing
aliasHelpDyAxisLogScaleY2 :: (TS.Text, WithHelp)
aliasHelpDyAxisLogScaleY2 =
    generalHelp aliasDyAxisLogScaleY2 "dyAxisLogScaleY2" "Toggles log scaling for the Y2 axis for the query results" Nothing
aliasHelpDyAxisIndependentTicksY :: (TS.Text, WithHelp)
aliasHelpDyAxisIndependentTicksY =
    generalHelp aliasDyAxisIndependentTicksY "dyAxisIndependentTicksY" "Toggles independent ticks for Y1 axis for the query results" Nothing
aliasHelpDyAxisIndependentTicksY2 :: (TS.Text, WithHelp)
aliasHelpDyAxisIndependentTicksY2 =
    generalHelp aliasDyAxisIndependentTicksY2 "dyAxisIndependentTicksY2" "Toggles independent ticks for Y2 axis for the query results" Nothing
aliasHelpDyAxisDrawAxisY :: (TS.Text, WithHelp)
aliasHelpDyAxisDrawAxisY =
    generalHelp aliasDyAxisDrawAxisY "dyAxisDrawAxisY" "Toggles drawing of Y1 axis for the query results" Nothing
aliasHelpDyAxisDrawAxisY2 :: (TS.Text, WithHelp)
aliasHelpDyAxisDrawAxisY2 =
    generalHelp aliasDyAxisDrawAxisY2 "dyAxisDrawAxisY2" "Toggles darwing of Y2 axis for the query results" Nothing
aliasHelpDyAxisIncludeZeroY :: (TS.Text, WithHelp)
aliasHelpDyAxisIncludeZeroY =
    generalHelp aliasDyAxisIncludeZeroY "dyAxisIncludeZeroY" "Toggles whether zero should be drawn for Y1 axis for the query results" Nothing
aliasHelpDyAxisIncludeZeroY2 :: (TS.Text, WithHelp)
aliasHelpDyAxisIncludeZeroY2 =
    generalHelp aliasDyAxisIncludeZeroY2 "dyAxisIncludeZeroY2" "Toggles whether zero should be drawn for Y2 axis for the query results" Nothing
aliasHelpDyGraphNulls :: (TS.Text, WithHelp)
aliasHelpDyGraphNulls =
    generalHelp aliasDyGraphNulls "dyGraphNulls" "Toggles graphing of nulls for the query results" Nothing
aliasHelpDyStepPlot :: (TS.Text, WithHelp)
aliasHelpDyStepPlot =
    generalHelp aliasDyStepPlot "dyStepPlot" "Toggles if graph should be step plotted for the query results" Nothing
aliasHelpDyForceStep :: (TS.Text, WithHelp)
aliasHelpDyForceStep =
    generalHelp aliasDyForceStep "dyForceStep" "Sets connectSeperatedPoints, graphNulls and stepPlot to toggled for the query results" Nothing
aliasHelpDyStepNulls :: (TS.Text, WithHelp)
aliasHelpDyStepNulls =
    generalHelp aliasDyStepNulls "dyStepNulls" "Toggles connectSeperatedPoints, graphNulls and stepPlot for the query results" Nothing
aliasHelpDyConstrainMin :: (TS.Text, WithHelp)
aliasHelpDyConstrainMin =
    generalHelp aliasDyConstrainMin "dyConstrainMin" "Sets the min constraint for the graphsurface Y1 axis" Nothing
aliasHelpDyConstrainMax :: (TS.Text, WithHelp)
aliasHelpDyConstrainMax =
    generalHelp aliasDyConstrainMax "dyConstrainMax" "Sets the max constraint for the graphsurface Y1 axis" Nothing
aliasHelpDyConstrainMinY2 :: (TS.Text, WithHelp)
aliasHelpDyConstrainMinY2 =
    generalHelp aliasDyConstrainMinY2 "dyConstrainMinY2" "Sets the min constraint for the graphsurface Y2 axis" Nothing
aliasHelpDyConstrainMaxY2 :: (TS.Text, WithHelp)
aliasHelpDyConstrainMaxY2 =
    generalHelp aliasDyConstrainMaxY2 "dyConstrainMaxY2" "Sets the max constraint for the graphsurface Y2 axis" Nothing
aliasHelpDyConstrain :: (TS.Text, WithHelp)
aliasHelpDyConstrain =
    generalHelp aliasDyConstrain "dyConstrain" "Sets the min and max constraint for the graphsurface Y1 axis" Nothing
aliasHelpDyConstrainY2 :: (TS.Text, WithHelp)
aliasHelpDyConstrainY2 =
    generalHelp aliasDyConstrainY2 "dyConstrainY2" "Sets the min and max constraint for the graphsurface Y2 axis" Nothing
aliasHelpDyRemoveMinConstraint :: (TS.Text, WithHelp)
aliasHelpDyRemoveMinConstraint =
    generalHelp aliasDyRemoveMinConstraint "dyRemoveMinConstraint" "Removes the min constraint for the graphsurface Y1 axis" Nothing
aliasHelpDyRemoveMaxConstraint :: (TS.Text, WithHelp)
aliasHelpDyRemoveMaxConstraint =
    generalHelp aliasDyRemoveMaxConstraint "dyRemoveMaxConstraint" "Removes the max constraint for the graphsurface Y1 axis" Nothing
aliasHelpDyRemoveMinConstraintY2 :: (TS.Text, WithHelp)
aliasHelpDyRemoveMinConstraintY2 =
    generalHelp aliasDyRemoveMinConstraintY2 "dyRemoveMinConstraintY2" "Removes the min constraint for the graphsurface Y2 axis" Nothing
aliasHelpDyRemoveMaxConstraintY2 :: (TS.Text, WithHelp)
aliasHelpDyRemoveMaxConstraintY2 =
    generalHelp aliasDyRemoveMaxConstraintY2 "dyRemoveMaxConstraintY2" "Removes the max constraint for the graphsurface Y2 axis" Nothing
aliasHelpDyRemoveConstraint :: (TS.Text, WithHelp)
aliasHelpDyRemoveConstraint =
    generalHelp aliasDyRemoveConstraint "dyRemoveConstraint" "Removes the min and max constraint for the graphsurface Y1 axis" Nothing
aliasHelpDyRemoveConstraintY2 :: (TS.Text, WithHelp)
aliasHelpDyRemoveConstraintY2 =
    generalHelp aliasDyRemoveConstraintY2 "dyRemoveConstraintY2" "Removes the min and max constraint for the graphsurface Y2 axis" Nothing
