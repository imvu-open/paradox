{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Paradox.Eval (
  Expr (..)
, evalWithEnv
, collectCounters
, filterCounters
, padding
, getSandboxedActions
, resolve
, resolveInternal
, SandboxedActionStatus(..)
, Padding (..)
) where

import Prelude hiding (floor, ceiling)
import qualified Prelude as P (floor)
import Paradox.Debug

import Text.Regex.PCRE
import Control.Exception                        (catch)
import Control.Monad.Reader                     ( Reader
                                                , runReader
                                                , asks
                                                )
import Control.Monad.Writer                     ( WriterT(..)
                                                , runWriterT
                                                , tell
                                                )
import Data.Monoid                              ((<>))
import Data.Either                              ( lefts
                                                , rights
                                                )
import Data.Maybe                               ( mapMaybe
                                                , catMaybes
                                                )
import Data.Map.Strict                          (Map)
import System.Process                           (readProcess)


import Language.Paradox.Eval.Types              ( Expr(..)
                                                , ExprType(..)
                                                , Accum(..)
                                                , LogEntry(..)
                                                , World(..)
                                                , Unresolved
                                                , WorldConstraints(..)
                                                , resolved
                                                )
import Paradox.Shell.Types                      ( UnsafeSandboxedAction(..)
                                                , ShellCommand(..)
                                                , ShellArgument(..)
                                                )
import Paradox.Logger.Global                    ( logInfoG
                                                , logErrorG
                                                , Log(..)
                                                )

import Paradox.Istatd.Types                     ( CounterName
                                                , CounterSpec (..)
                                                , CounterSpecIntermediate(..)
                                                , CounterNameParts(..)
                                                , ParadoxTimeSeries(..)
                                                , CounterNameInParts
                                                , SafeCounterNameInParts
                                                , DrawOptions(..)
                                                , DrawOption(..)
                                                , ReturnData
                                                , ParadoxReturn(..)
                                                , TimeSeriesChunk(..)
                                                , ErrorWrapper (..)
                                                )
import Paradox.Istatd.Functions                 ( convertToSafeNameInPartsMany
                                                , finalizeIntermediateCounterSpec
                                                )

import Paradox.Types                            (CommandConfig(..))
import Paradox.Istatd.CounterMap                ( lookupMagic
                                                , convertForSearch
                                                , unpackSearch
                                                , deMaybe
                                                )
import Paradox.Functions.Util                   (Summarizer)
import Paradox.Util                             (SimplePartitionable(..))

import qualified Data.Map.Strict                as M
import qualified Paradox.Functions              as Fn
import qualified Data.Text                      as TS
import qualified Data.Text.Encoding             as TSE
import qualified Data.Set                       as S
import qualified Data.ByteString.Lazy.Char8     as BSLC
import qualified Data.Foldable                  as Fold
import qualified Data.Semigroup                 as Semi
import qualified Data.List.NonEmpty             as NonEmpty



idTimeSeries :: WorldConstraints
             -> a
             -> a
idTimeSeries = const id

aliasTimeSeries :: WorldConstraints
                -> CounterName
                -> ReturnData
                -> ReturnData
aliasTimeSeries = Fn.alias

smartAliasTimeSeries :: WorldConstraints
                     -> CounterName
                     -> Int
                     -> ReturnData
                     -> ReturnData
smartAliasTimeSeries = Fn.smartAlias

smartAliasATimeSeries :: WorldConstraints
                      -> CounterName
                      -> [Int]
                      -> ReturnData
                      -> ReturnData
smartAliasATimeSeries = Fn.smartAliasA

excludeTimeSeries :: WorldConstraints
                  -> CounterName
                  -> ReturnData
                  -> ReturnData
excludeTimeSeries = Fn.exclude

includeTimeSeries :: WorldConstraints
                  -> CounterName
                  -> ReturnData
                  -> ReturnData
includeTimeSeries = Fn.include

constantLine :: WorldConstraints
             -> Double
             -> ReturnData
constantLine = Fn.constantLine

scaleTimeSeries :: WorldConstraints
                -> Double
                -> ReturnData
                -> ReturnData
scaleTimeSeries = Fn.scale

scalarDivideTimeSeries :: WorldConstraints
                       -> Double
                       -> ReturnData
                       -> ReturnData
scalarDivideTimeSeries = Fn.scalarDivide

scaleByIntervalTimeSeries :: WorldConstraints
                          -> ReturnData
                          -> ReturnData
scaleByIntervalTimeSeries = Fn.scaleByInterval

divideByIntervalTimeSeries :: WorldConstraints
                           -> ReturnData
                           -> ReturnData
divideByIntervalTimeSeries = Fn.divideByInterval

subTimeSeries :: WorldConstraints
              -> ReturnData
              -> ReturnData
              -> ReturnData
subTimeSeries = Fn.subtractify

divideTimeSeries :: WorldConstraints
                 -> ReturnData
                 -> ReturnData
                 -> ReturnData
divideTimeSeries = Fn.divideTimeSeries

consTimeSeries :: WorldConstraints
               -> ReturnData
               -> ReturnData
               -> ReturnData
consTimeSeries = Fn.cons

sumTimeSeries :: WorldConstraints
              -> ReturnData
              -> ReturnData
sumTimeSeries = Fn.sumify

graphField :: WorldConstraints
           -> (TS.Text, TimeSeriesChunk -> Double)
           -> ReturnData
           -> ReturnData
graphField = Fn.graphField

maxAtEachSample :: WorldConstraints
                -> (TS.Text, TimeSeriesChunk -> Double)
                -> ReturnData
                -> ReturnData
maxAtEachSample = Fn.maxAtSamples

summarize :: Summarizer
          -> WorldConstraints
          -> Int
          -> ReturnData
          -> ReturnData
summarize = Fn.summarize

avgTimeSeries :: WorldConstraints
              -> ReturnData
              -> ReturnData
avgTimeSeries = Fn.averageTimeSeries

absTimeSeries :: WorldConstraints
              -> ReturnData
              -> ReturnData
absTimeSeries = Fn.absTimeSeries

movingAverageTimeSeries :: WorldConstraints
                        -> Int
                        -> ReturnData
                        -> ReturnData
movingAverageTimeSeries = Fn.movingAverage

medianFilterTimeSeries :: WorldConstraints
                       -> Int
                       -> ReturnData
                       -> ReturnData
medianFilterTimeSeries = Fn.medianFilter

medianAbsoluteDeviationTimeSeries :: WorldConstraints
                       -> ReturnData
                       -> ReturnData
medianAbsoluteDeviationTimeSeries = Fn.medianAbsoluteDeviation

medianAbsoluteDeviationFilterTimeSeries :: WorldConstraints
                       -> Int
                       -> ReturnData
                       -> ReturnData
medianAbsoluteDeviationFilterTimeSeries = Fn.medianAbsoluteDeviationFilter

doubleMedianAbsoluteDeviationTimeSeries :: WorldConstraints
                       -> ReturnData
                       -> ReturnData
doubleMedianAbsoluteDeviationTimeSeries = Fn.doubleMedianAbsoluteDeviation

doubleMedianAbsoluteDeviationFilterTimeSeries :: WorldConstraints
                       -> Int
                       -> ReturnData
                       -> ReturnData
doubleMedianAbsoluteDeviationFilterTimeSeries = Fn.doubleMedianAbsoluteDeviationFilter

loess :: WorldConstraints
      -> Double
      -> ReturnData
      -> ReturnData
loess = Fn.loess

edmMulti :: WorldConstraints
      -> Int
      -> Double
      -> ReturnData
      -> ReturnData
edmMulti = Fn.edmMulti

stl :: WorldConstraints
      -> ReturnData
      -> ReturnData
stl = Fn.stl

stlSeason :: WorldConstraints
      -> ReturnData
      -> ReturnData
stlSeason = Fn.stlSeason

stlTrend :: WorldConstraints
      -> ReturnData
      -> ReturnData
stlTrend = Fn.stlTrend

stlResidual :: WorldConstraints
      -> ReturnData
      -> ReturnData
stlResidual = Fn.stlResidual

offsetTimeSeries :: WorldConstraints
                 -> Double
                 -> ReturnData
                 -> ReturnData
offsetTimeSeries = Fn.offset

keepSeriesAboveDeviation :: WorldConstraints
                         -> Int
                         -> ReturnData
                         -> ReturnData
keepSeriesAboveDeviation = Fn.keepSeriesAboveDeviation

keepSeriesAboveDeviationInc :: WorldConstraints
                            -> Int
                            -> ReturnData
                            -> ReturnData
keepSeriesAboveDeviationInc = Fn.keepSeriesAboveDeviationInc

keepSeriesWithinDeviation :: WorldConstraints
                          -> Int
                          -> ReturnData
                          -> ReturnData
keepSeriesWithinDeviation = Fn.keepSeriesWithinDeviation

keepSeriesWithinDeviationInc :: WorldConstraints
                             -> Int
                             -> ReturnData
                             -> ReturnData
keepSeriesWithinDeviationInc = Fn.keepSeriesWithinDeviationInc

removeAbovePercentile :: WorldConstraints
                      -> Int
                      -> ReturnData
                      -> ReturnData
removeAbovePercentile = Fn.removeAbovePercentile

removeAbovePercentileInc :: WorldConstraints
                         -> Int
                         -> ReturnData
                         -> ReturnData
removeAbovePercentileInc = Fn.removeAbovePercentileInc

removeBelowPercentile :: WorldConstraints
                      -> Int
                      -> ReturnData
                      -> ReturnData
removeBelowPercentile = Fn.removeBelowPercentile

removeBelowPercentileInc :: WorldConstraints
                         -> Int
                         -> ReturnData
                         -> ReturnData
removeBelowPercentileInc = Fn.removeBelowPercentileInc

removeAboveValue :: WorldConstraints
                 -> Double
                 -> ReturnData
                 -> ReturnData
removeAboveValue = Fn.removeAboveValue

removeAboveValueInc :: WorldConstraints
                    -> Double
                    -> ReturnData
                    -> ReturnData
removeAboveValueInc = Fn.removeAboveValueInc

removeBelowValue :: WorldConstraints
                 -> Double
                 -> ReturnData
                 -> ReturnData
removeBelowValue = Fn.removeBelowValue

removeBelowValueInc :: WorldConstraints
                    -> Double
                    -> ReturnData
                    -> ReturnData
removeBelowValueInc = Fn.removeBelowValueInc

ceiling :: WorldConstraints
        -> Double
        -> ReturnData
        -> ReturnData
ceiling = Fn.ceiling

floor :: WorldConstraints
      -> Double
      -> ReturnData
      -> ReturnData
floor = Fn.floor

keepSeriesAbove :: WorldConstraints
                -> Double
                -> ReturnData
                -> ReturnData
keepSeriesAbove = Fn.keepSeriesAbove

keepSeriesAboveInc :: WorldConstraints
                   -> Double
                   -> ReturnData
                   -> ReturnData
keepSeriesAboveInc = Fn.keepSeriesAboveInc

keepSeriesBelow :: WorldConstraints
                -> Double
                -> ReturnData
                -> ReturnData
keepSeriesBelow = Fn.keepSeriesBelow

keepSeriesBelowInc :: WorldConstraints
                   -> Double
                   -> ReturnData
                   -> ReturnData
keepSeriesBelowInc = Fn.keepSeriesBelowInc

keepSeriesAllAbove :: WorldConstraints
                   -> Double
                   -> ReturnData
                   -> ReturnData
keepSeriesAllAbove = Fn.keepSeriesAllAbove

keepSeriesAllAboveInc :: WorldConstraints
                      -> Double
                      -> ReturnData
                      -> ReturnData
keepSeriesAllAboveInc = Fn.keepSeriesAllAboveInc

keepSeriesAllBelow :: WorldConstraints
                   -> Double
                   -> ReturnData
                   -> ReturnData
keepSeriesAllBelow = Fn.keepSeriesAllBelow

keepSeriesAllBelowInc :: WorldConstraints
                      -> Double
                      -> ReturnData
                      -> ReturnData
keepSeriesAllBelowInc = Fn.keepSeriesAllBelowInc

integrateTimeSeries :: WorldConstraints
                    -> ReturnData
                    -> ReturnData
integrateTimeSeries = Fn.integrify

deriveTimeSeries :: WorldConstraints
                 -> ReturnData
                 -> ReturnData
deriveTimeSeries = Fn.derivify

logTimeSeries :: WorldConstraints
              -> Int
              -> Double
              -> Int
              -> ReturnData
              -> ReturnData
logTimeSeries = Fn.logP

mapHO :: WorldConstraints
     -> (ReturnData -> ReturnData)
     -> ReturnData
     -> ReturnData
mapHO _w fn istatdData =
    let istatdDatas = M.foldrWithKey (\k a b -> istatdData { timeSeriesMap = M.fromList [(k,a)]} : b) [] (timeSeriesMap istatdData)
        results = map fn istatdDatas
        reduced = foldr (\a b -> b { timeSeriesMap = timeSeriesMap b <> timeSeriesMap a
                                   , options = options b <> options a
                                   , axisOptions = axisOptions b <> axisOptions a
                                   }
                        )
                        (istatdData { timeSeriesMap = mempty})
                        results
    in reduced

filterCounters :: Expr a
               -> [CounterSpec]
               -> [CounterSpec]
filterCounters e c = case e of
    Apply f a -> case f of
        Include ->
            let regex = case a of
                    CounterN r -> r
                    _ -> ""
                filt ctr = TSE.encodeUtf8 (csName ctr) =~ TSE.encodeUtf8 regex
            in filter filt c
        Exclude ->
            let regex = case a of
                    CounterN r -> r
                    _ -> ""
                filt ctr = not $ TSE.encodeUtf8 (csName ctr) =~ TSE.encodeUtf8 regex
            in filter filt c
        _ -> let cs' = filterCounters f c
                 cs'' = filterCounters a cs'
             in cs''
    a :|: f -> case f of
        Include ->
            let regex = case a of
                    CounterN r -> r
                    _ -> ""
                filt ctr = TSE.encodeUtf8 (csName ctr) =~ TSE.encodeUtf8 regex
            in filter filt c
        Exclude ->
            let regex = case a of
                    CounterN r -> r
                    _ -> ""
                filt ctr = not $ TSE.encodeUtf8 (csName ctr) =~ TSE.encodeUtf8 regex
            in filter filt c
        _ -> let cs' = filterCounters f c
                 cs'' = filterCounters a cs'
             in cs''
    _ -> c

collectCounters :: Expr a
                -> [CounterSpec]
collectCounters = \case
    x :|: y    -> collectCounters x ++ collectCounters y
    x :.: y    -> collectCounters x ++ collectCounters y
    x :>>>: y    -> collectCounters x ++ collectCounters y
    x :...: y    -> collectCounters x ++ collectCounters y
    Apply x y    -> collectCounters x ++ collectCounters y
    Counter xs  -> xs
    _ -> []

data Padding = Buckets Int | TimeSpan Int


padding :: Expr a
        -> [(Padding, Padding)]
padding = \case
    Apply f a -> case f of
        MovingAverage -> paddingL a
        MedianFilter -> paddingLR a
        _ -> padding f ++ padding a
    a :|: f -> case f of
        MovingAverage -> paddingL a
        MedianFilter -> paddingLR a
        _ -> padding f ++ padding a
    x :.: y -> padding x ++ padding y -- This doesnt seem complete....
    x :>>>: y -> padding x ++ padding y -- This doesnt seem complete....
    x :...: y -> padding x ++ padding y -- This doesnt seem complete....
    Stl -> [(TimeSpan $ 86400 * 7,Buckets 0)]
    StlSeason -> [(TimeSpan $ 86400 * 7,Buckets 0)]
    StlTrend -> [(TimeSpan $ 86400 * 7,Buckets 0)]
    StlResidual -> [(TimeSpan $ 86400 * 7,Buckets 0)]
    _ -> []

paddingL :: Expr a
         -> [(Padding, Padding)]
paddingL = \case
    NumI x -> [(Buckets x, Buckets 0)]
    x -> padding x

paddingLR :: Expr a
          -> [(Padding, Padding)]
paddingLR = \case
    NumI x -> [(Buckets x, Buckets x)]
    x -> padding x

-- | Resolve a @CounterSpecIntermediate@ expression by using confgured
-- whitelisted sandboxed shell actions to replace parts of counters.
resolveInternal :: [CounterSpecIntermediate]
                -> [SandboxedActionStatus]
                -> [SandboxedActionStatus]
                -> Either ErrorWrapper (Expr ReturnData)
resolveInternal csis failures successes =
        traceIt ("resolveInternal csis:"
                ++ show csis
                ++ "\nfails: "
                ++ show failures
                ++ "\nsucc: "
                ++ show successes)
        $ case failures of
    -- If we have no failures we want to modify the expression
    -- currenytl being worked on to be a real Counter rather than
    -- CounterIntermediate
    [] ->
        let saMap :: M.Map UnsafeSandboxedAction [CounterName]
            saMap = M.fromList [el | SandboxedActionSuccess el <- successes]
            csis' = map (\csi ->
                    let nameParts :: CounterNameInParts
                        nameParts = csiName csi

                        -- When we find the name parts that require
                        -- replacement we need to look up what
                        -- result was for them.
                        replaceNamePart :: CounterNameParts
                                        -> Maybe [CounterName]
                        replaceNamePart = \case
                            CNPShellReplacement r -> do
                                (f:fs) <- case TS.splitOn " " r of
                                            [] -> Nothing
                                            t -> Just t

                                let val :: Maybe [CounterName]
                                    val = M.lookup (UnsafeSandboxedAction f fs) saMap
                                val
                            _ -> Nothing

                        nameParts' :: [Maybe [CounterName]]
                        nameParts' = map replaceNamePart nameParts
                    in nameParts'
                ) csis :: [[Maybe [CounterName]]]
            -- Given counter apache.{'echo -n response-time\nresponse'}.host.{'hostname'}
            -- We will end up with [Str,Dot,Repl,Dot,Str,Dot,Repl]
            -- our replacements lengths will be  [Nothing, Nothing, Just 2, Nothing,Nothing,Nothing, Just N]
            -- this means our final list of counters will be replicate 2*N
            --
            pairs :: [(CounterSpecIntermediate, [Maybe [CounterName]])]
            pairs = zip csis csis'

            finals :: Either ErrorWrapper (Expr ReturnData)
            finals =
                let rets = map convert pairs
                    anyFailed = lefts rets
                in case anyFailed of
                       [] -> Right $ Counter $ concat $ rights rets
                       fails -> Left $ MkErrorWrapper "Couldn't resolve expression" fails

            convert :: (CounterSpecIntermediate, [Maybe [CounterName]])
                    -> Either ErrorWrapper [CounterSpec]
            convert (csi, possibleReplacements) =
                let nameParts :: CounterNameInParts
                    nameParts = csiName csi

                    replacePairs :: [(CounterNameParts, Maybe [CounterName])]
                    replacePairs = zip nameParts possibleReplacements

                    replicatedAndReplaced :: Either ErrorWrapper [SafeCounterNameInParts]
                    replicatedAndReplaced = convertToSafeNameInPartsMany
                                          $ map reverse
                                          $ Fold.foldl' replicateAndReplace [mempty] replacePairs


                    replicateAndReplace :: [CounterNameInParts]
                                        -> (CounterNameParts, Maybe [CounterName])
                                        -> [CounterNameInParts]
                    replicateAndReplace acc = \case
                        (part, Nothing) -> map (part:) acc
                        (_, Just replacement) ->
                            let replace ::  [[CounterNameParts]
                                        -> [CounterNameParts]]
                                replace = map ((:) . CNPShellReplaced)
                                    -- This is kinda ugly. Since
                                    -- another replacement can have
                                    -- affected acc, we need to
                                    -- replicate each of our
                                    -- replacements (seperately so that the
                                    -- strides match) to match the
                                    -- new stride. There is
                                    -- probably a nicer way to do
                                    -- this.
                                    (concatMap (replicate (length acc)) replacement)

                                resultingList :: [CounterNameInParts]
                                resultingList = zipWith ($)
                                                        replace
                                                        (concat $ replicate (length replacement) acc)
                            in resultingList

                    toCounterSpecs :: Either ErrorWrapper [CounterSpec]
                    toCounterSpecs = fmap (finalizeIntermediateCounterSpec csi) replicatedAndReplaced
                in toCounterSpecs
        in finals
    x -> Left $ MkErrorWrapper "Failures performaing sanboxed actions" [e | SandboxedActionError e <- x]

-- | Resolve an @Unresolved@ Expr a in IO. Expressions start as unresolved
-- since there may be external state needed to transform them so they are
-- ready to be evaluated in their pure environment
resolve :: Maybe (M.Map TS.Text CommandConfig)
        -> Unresolved (Expr a)
        -> IO (Either ErrorWrapper (Expr a))
resolve m expr = resolve' m (resolved expr)

errorWrappers :: (Either ErrorWrapper (Expr a), Either ErrorWrapper (Expr b))
              -> [ErrorWrapper]
errorWrappers (Left l, Left r) = [l, r]
errorWrappers (Left l, _) = [l]
errorWrappers (_ ,Left r) = [r]
errorWrappers (_, _) = []

-- | Internal resolve recursivevely resolves @Expr@ parts
-- replacing/augmenting those that need IO in order to be ready for use.
resolve' :: Maybe (M.Map TS.Text CommandConfig)
         -> Expr a
         -> IO (Either ErrorWrapper (Expr a))
resolve' m = \case
    CounterIntermediate csis -> do
        -- Shells out and performs commands that get looked up in the
        -- provided map from configuration
        res <- sandboxedActions m csis
        let (failures,successes) = spartition res
        return $ resolveInternal csis failures successes
    Apply x y -> do
        x' <- resolve' m x
        y' <- resolve' m y
        case (x', y') of
            (Right x'', Right y'') -> return $ Right $ Apply x'' y''
            err -> return $ Left $ MkErrorWrapper "Failed to resolve during apply" (errorWrappers err)
    x :|: y -> do
        x' <- resolve' m x
        y' <- resolve' m y
        case (x', y') of
            (Right x'', Right y'') -> return $ Right $ x'' :|: y''
            err -> return $ Left $ MkErrorWrapper "Failed to resolve during infix apply" (errorWrappers err)
    x :.: y -> do
        x' <- resolve' m x
        y' <- resolve' m y
        case (x', y') of
            (Right x'', Right y'') -> return $ Right $ x'' :.: y''
            err -> return $ Left $ MkErrorWrapper "Failed to resolve during composition" (errorWrappers err)
    x :>>>: y -> do
        x' <- resolve' m x
        y' <- resolve' m y
        case (x', y') of
            (Right x'', Right y'') -> return $ Right $ x'' :>>>: y''
            err -> return $ Left $ MkErrorWrapper "Failed to resolve during internal distribute" (errorWrappers err)
    x :...: y -> do
        x' <- resolve' m x
        y' <- resolve' m y
        case (x', y') of
            (Right x'', Right y'') -> return $ Right $ x'' :...: y''
            err -> return $ Left $ MkErrorWrapper "Failed to resolve during internal composition(chain)" (errorWrappers err)
    x -> return $ Right x

data SandboxedActionStatus = SandboxedActionSuccess (UnsafeSandboxedAction, [CounterName])
                           | SandboxedActionError TS.Text

instance Show SandboxedActionStatus where
    show (SandboxedActionSuccess (usa, cs)) = "SandboxedActionSuccess " ++ show usa ++ " " ++ show cs
    show (SandboxedActionError s) = "SandboxedActionError" ++ show s

instance SimplePartitionable SandboxedActionStatus where
    spivot = \case
        x@(SandboxedActionError _) -> Left x
        x@(SandboxedActionSuccess _) -> Right x

sandboxedActions :: Maybe (Map TS.Text CommandConfig)
                 -> [CounterSpecIntermediate]
                 -> IO [SandboxedActionStatus]
sandboxedActions sm csis = do
    let sandboxedActions' :: [UnsafeSandboxedAction]
        sandboxedActions' = S.toList $ S.fromList $ getSandboxedActions csis

        lookupSandboxedAction :: UnsafeSandboxedAction
                              -> Maybe (UnsafeSandboxedAction, CommandConfig)
        lookupSandboxedAction usa@(UnsafeSandboxedAction cmd _) =
            case sm of
                Nothing -> Nothing
                Just sm' ->  case M.lookup cmd sm' of
                    Nothing -> Nothing
                    Just sa@CommandConfig {..}-> Just (usa, sa) -- should differentiate with types

    mapM (\(usa@UnsafeSandboxedAction{..}, CommandConfig{..}) ->
        catch
            (do
                let command = TS.unpack . unShellCommand $ ccCommand
                    defArgs = map (TS.unpack . unShellArgument) ccArgs
                    ourArgs = map TS.unpack usaRest
                    allArgs = defArgs ++ ourArgs

                logInfoG $ LogText $ "Doing a sandboxed action: "
                                  <> TS.pack (show command)
                                  <> (TS.pack . show $ map (BSLC.pack . show) allArgs)

                ret <- readProcess command allArgs ""

                let result = TS.splitOn ccDelim . TS.strip . TS.pack $ ret

                return $ SandboxedActionSuccess (usa, result)
            )
            (\e -> do
                let errorParts = ["Exception thrown on action ", show usa, "\n", show (e :: IOError)]
                    errorMsg = "Failed a sandboxed action!: " <> foldr1 (<>) (map TS.pack errorParts)

                logErrorG $ LogText errorMsg

                return $ SandboxedActionError errorMsg
            )
      ) $ mapMaybe lookupSandboxedAction sandboxedActions'

getSandboxedActions :: [CounterSpecIntermediate]
                    -> [UnsafeSandboxedAction]
getSandboxedActions csis =
    let extractedParts = concatMap csiName csis
        filtered = filter (\x -> case x of
                                    CNPShellReplacement {} -> True
                                    _ -> False) extractedParts
        toSandboxedAction :: CounterNameParts
                          -> Maybe UnsafeSandboxedAction
        toSandboxedAction (CNPShellReplacement sr) = do
            (f:fs) <- case TS.splitOn " " sr of
                        [] -> Nothing
                        t -> Just t
            return UnsafeSandboxedAction { usaCmd = f, usaRest = fs }
        toSandboxedAction _ = Nothing
    in mapMaybe toSandboxedAction filtered

whenElseTransformed :: (ReturnData -> Bool)
                    -> (ReturnData -> ReturnData)
                    -> (ReturnData -> ReturnData)
                    -> (ReturnData -> ReturnData)
                    -> ReturnData
                    -> ReturnData
whenElseTransformed predicate trans act elseAct tsm = if predicate (trans tsm) then act tsm
                                                                           else elseAct tsm
whenTransformed :: (ReturnData -> Bool)
                -> (ReturnData -> ReturnData)
                -> (ReturnData -> ReturnData)
                -> ReturnData
                -> ReturnData
whenTransformed p xf a = whenElseTransformed p xf a id

whenElse :: (ReturnData -> Bool)
         -> (ReturnData -> ReturnData)
         -> (ReturnData -> ReturnData)
         -> ReturnData
         -> ReturnData
whenElse p = whenElseTransformed p id

when :: (ReturnData -> Bool)
     -> (ReturnData -> ReturnData)
     -> ReturnData
     -> ReturnData
when p a = whenElse p a id

applyWithValue :: (Double -> ReturnData -> ReturnData)
               -> (ReturnData -> Double)
               -> ReturnData
               -> ReturnData
applyWithValue act find tsm = let res = find tsm
                              in act res tsm

getValueInRange :: (TS.Text, TimeSeriesChunk -> Double)
                -> (Double -> Double -> Double)
                -> Double
                -> ReturnData
                -> Double
getValueInRange getter selector pctRange tsm =
    let (_fname, f) = getter
        clean = Fold.foldr1 (Semi.<>) $ catMaybes $ ranges f pctRange tsm
        found = Fold.foldr1 selector clean
    in found

getLargestInRange :: (TS.Text, TimeSeriesChunk -> Double)
                  -> Double
                  -> ReturnData
                  -> Double
getLargestInRange g = getValueInRange g max

getLargest :: (TS.Text, TimeSeriesChunk -> Double)
           -> ReturnData
           -> Double
getLargest g = getLargestInRange g 100.0

getSmallestInRange :: (TS.Text, TimeSeriesChunk -> Double)
                   -> Double
                   -> ReturnData
                   -> Double
getSmallestInRange g = getValueInRange g min

getSmallest :: (TS.Text, TimeSeriesChunk -> Double)
            -> ReturnData
            -> Double
getSmallest g = getSmallestInRange g 100.0

hasAnyValue :: (Double -> Double -> Bool)
            -> (TS.Text, TimeSeriesChunk -> Double)
            -> Double
            -> ReturnData
            -> Bool
hasAnyValue fnp g v = let op = hasValueInRange id Fold.any Fold.any fnp g v 100.0
                      in op

hasAllValue :: (Double -> Double -> Bool)
            -> (TS.Text, TimeSeriesChunk -> Double)
            -> Double
            -> ReturnData
            -> Bool
hasAllValue fnp g v = let op = hasValueInRange id Fold.any Fold.any fnp g v 100.0
                      in op

hasNoValue :: (Double -> Double -> Bool)
           -> (TS.Text, TimeSeriesChunk -> Double)
           -> Double
           -> ReturnData
           -> Bool
hasNoValue fnp g v = let op = hasValueInRange not Fold.any Fold.any fnp g v 100.0
                     in op

hasAnyValueInRange :: (Double -> Double -> Bool)
                   -> (TS.Text, TimeSeriesChunk -> Double)
                   -> Double
                   -> Double
                   -> ReturnData
                   -> Bool
hasAnyValueInRange fnp g v r = let op = hasValueInRange id Fold.any Fold.any fnp g v r
                               in op

hasAllValueInRange :: (Double -> Double -> Bool)
                   -> (TS.Text, TimeSeriesChunk -> Double)
                   -> Double
                   -> Double
                   -> ReturnData
                   -> Bool
hasAllValueInRange fnp g v r = let op = hasValueInRange id Fold.all Fold.all fnp g v r
                               in op

hasNoValueInRange :: (Double -> Double -> Bool)
                  -> (TS.Text, TimeSeriesChunk -> Double)
                  -> Double
                  -> Double
                  -> ReturnData
                  -> Bool
hasNoValueInRange fnp g v r = let op = hasValueInRange not Fold.any Fold.any fnp g v r
                              in op

hasValueInRange :: (Bool -> Bool)
                -> ((NonEmpty.NonEmpty Double -> Bool) ->  [NonEmpty.NonEmpty Double] -> Bool)
                -> ((Double -> Bool) -> NonEmpty.NonEmpty Double -> Bool)
                -> (Double -> Double -> Bool)
                -> (TS.Text, TimeSeriesChunk -> Double)
                -> Double
                -> Double
                -> ReturnData
                -> Bool
hasValueInRange n op ip fnp g v pctRange =
    let (_fname, f) = g
        op' tsm = n $ op (ip (fnp v) ) (catMaybes $ ranges f pctRange tsm)
        in op'

ranges
  :: (TimeSeriesChunk -> a)
  -> Double -> ParadoxReturn -> [Maybe (NonEmpty.NonEmpty a)]
ranges f pctRange tsm = M.elems
  $ fmap  (\ts -> let d = ptsData ts
                      l = NonEmpty.length d
                      r = P.floor (((100.0 - pctRange) / 100.0) * fromIntegral l)
                      p = NonEmpty.drop r d
                      p' :: Maybe (NonEmpty.NonEmpty TimeSeriesChunk)
                      p' = NonEmpty.nonEmpty p
                  in fmap (fmap f) p'
          )
          (timeSeriesMap tsm)

-- | Evaluate an @Expr@ using the world state
evalWithEnv :: World
            -> Expr a
            -> (a, Accum)
evalWithEnv env expS = runReader (runWriterT $ evalReader expS) env

evalReader :: Expr a
           -> WriterT Accum (Reader World) a
evalReader ex = do
    let res = case ex of
            CounterN x  -> return x
            NumD x  -> return x
            NumI x  -> return x
            BoolC x  -> return x
            NumIA x  -> return x
            NumDA x  -> return x
            StrA x  -> return x
            BoolCA x  -> return x
            Id -> askFn idTimeSeries
            Sum -> askFn sumTimeSeries
            GraphField -> askFn $ \f -> graphField f
            MaxAtEachSample -> askFn $ \f -> maxAtEachSample f
            Summarize s -> askFn $ summarize s
            Avg -> askFn avgTimeSeries
            Abs -> askFn absTimeSeries
            MovingAverage -> askFn movingAverageTimeSeries
            MedianFilter -> askFn medianFilterTimeSeries
            MedianAbsoluteDeviationFilter -> askFn medianAbsoluteDeviationFilterTimeSeries
            MedianAbsoluteDeviation -> askFn medianAbsoluteDeviationTimeSeries
            DoubleMedianAbsoluteDeviationFilter -> askFn doubleMedianAbsoluteDeviationFilterTimeSeries
            DoubleMedianAbsoluteDeviation -> askFn doubleMedianAbsoluteDeviationTimeSeries
            Loess -> askFn loess
            EDMMulti -> askFn edmMulti
            Stl -> askFn stl
            StlSeason -> askFn stlSeason
            StlTrend -> askFn stlTrend
            StlResidual -> askFn stlResidual
            RemoveAbovePercentile -> askFn removeAbovePercentile
            RemoveAbovePercentileInc -> askFn removeAbovePercentileInc
            RemoveBelowPercentile -> askFn removeBelowPercentile
            RemoveBelowPercentileInc -> askFn removeBelowPercentileInc
            RemoveAboveValue -> askFn removeAboveValue
            RemoveAboveValueInc -> askFn removeAboveValueInc
            RemoveBelowValue -> askFn removeBelowValue
            RemoveBelowValueInc -> askFn removeBelowValueInc
            Ceiling -> askFn ceiling
            Floor -> askFn floor
            KeepSeriesAbove -> askFn keepSeriesAbove
            KeepSeriesAboveInc -> askFn keepSeriesAboveInc
            KeepSeriesBelow -> askFn keepSeriesBelow
            KeepSeriesBelowInc -> askFn keepSeriesBelowInc
            KeepSeriesAllAbove -> askFn keepSeriesAllAbove
            KeepSeriesAllAboveInc -> askFn keepSeriesAllAboveInc
            KeepSeriesAllBelow -> askFn keepSeriesAllBelow
            KeepSeriesAllBelowInc -> askFn keepSeriesAllBelowInc
            Offset -> askFn offsetTimeSeries
            KeepSeriesAboveDeviation -> askFn keepSeriesAboveDeviation
            KeepSeriesAboveDeviationInc -> askFn keepSeriesAboveDeviationInc
            KeepSeriesWithinDeviation -> askFn keepSeriesWithinDeviation
            KeepSeriesWithinDeviationInc -> askFn keepSeriesWithinDeviationInc
            Sub -> askFn subTimeSeries
            Divide -> askFn divideTimeSeries
            Cons -> askFn consTimeSeries
            Integrate -> askFn integrateTimeSeries
            Derive -> askFn deriveTimeSeries
            Log -> askFn logTimeSeries
            Scale -> askFn scaleTimeSeries
            ScalarDivide -> askFn scalarDivideTimeSeries
            ScaleByInterval -> askFn scaleByIntervalTimeSeries
            DivideByInterval -> askFn divideByIntervalTimeSeries
            Alias -> askFn aliasTimeSeries
            SmartAlias -> askFn smartAliasTimeSeries
            SmartAliasA -> askFn smartAliasATimeSeries
            Exclude -> askFn excludeTimeSeries
            Include -> askFn includeTimeSeries
            ConstantLine -> askFn constantLine
            x :|: f   -> do
                x' <- evalReader x
                f' <- evalReader f
                return $ f' x'
            Apply f x   -> do
                f' <- evalReader f
                x' <- evalReader x
                return $ f' x'
            f :.: fn   -> do
                f' <- evalReader f
                fn' <- evalReader fn
                return $ fn' . f'
            f :>>>: fn   -> do
                f' <- evalReader f
                fn' <- evalReader fn
                return $ \x -> fn' x . f' x
            f :...: fn   -> do
                f' <- evalReader f
                fn' <- evalReader fn
                return $ \x y -> fn' y . f' x
            Counter cs -> do
                tsm <- asks (\World {wMap} ->
                    M.fromList $
                    unpackSearch $
                    deMaybe $
                    concatMap (\x ->
                        lookupMagic (convertForSearch x) wMap
                    ) cs )
                return ParadoxReturn { options = mempty, axisOptions = mempty, timeSeriesMap = tsm }
            GraphOpts ->  return $
                let attachOpts js pts = pts { ptsDrawOptions = RawOptions js }
                in \js pt@ParadoxReturn { timeSeriesMap = tsm} -> pt { timeSeriesMap = M.map (attachOpts js) tsm }

            GraphOptI -> return $
                let v' = DrawOptionI
                in \k v tsm -> generalGraphOpt k (v' v) tsm
            GraphOptD -> return $
                let v' = DrawOptionD
                in \k v tsm -> generalGraphOpt k (v' v) tsm
            GraphOptS -> return $
                let v' = DrawOptionS
                in \k v tsm -> generalGraphOpt k (v' v) tsm
            GraphOptB -> return $
                let v' = DrawOptionB
                in \k v tsm -> generalGraphOpt k (v' v) tsm
            GraphOptIA -> return $
                let v' = DrawOptionIA
                in \k v tsm -> generalGraphOpt k (v' v) tsm
            GraphOptDA -> return $
                let v' = DrawOptionDA
                in \k v tsm -> generalGraphOpt k (v' v) tsm
            GraphOptSA -> return $
                let v' = DrawOptionSA
                in \k v tsm -> generalGraphOpt k (v' v) tsm
            GraphOptBA -> return $
                let v' = DrawOptionBA
                in \k v tsm -> generalGraphOpt k (v' v) tsm

            AxisOpts ->  return $
                let newOpts = RawOptions
                in \js pt@ParadoxReturn { axisOptions = axisOpts} -> pt { axisOptions = axisOpts <> newOpts js }

            AxisOptI -> return $
                let v' = DrawOptionI
                in \ax k v tsm -> generalAxisOpt ax k (v' v) tsm
            AxisOptD -> return $
                let v' = DrawOptionD
                in \ax k v tsm -> generalAxisOpt ax k (v' v) tsm
            AxisOptS -> return $
                let v' = DrawOptionS
                in \ax k v tsm -> generalAxisOpt ax k (v' v) tsm
            AxisOptB -> return $
                let v' = DrawOptionB
                in \ax k v tsm -> generalAxisOpt ax k (v' v) tsm
            AxisOptIA -> return $
                let v' = DrawOptionIA
                in \ax k v tsm -> generalAxisOpt ax k (v' v) tsm
            AxisOptDA -> return $
                let v' = DrawOptionDA
                in \ax k v tsm -> generalAxisOpt ax k (v' v) tsm
            AxisOptSA -> return $
                let v' = DrawOptionSA
                in \ax k v tsm -> generalAxisOpt ax k (v' v) tsm
            AxisOptBA -> return $
                let v' = DrawOptionBA
                in \ax k v tsm -> generalAxisOpt ax k (v' v) tsm

            SurfaceOpts ->  return $
                let newOpts = RawOptions
                in \js pt@ParadoxReturn { options = opts} -> pt { options = opts <> newOpts js }

            SurfaceOpt -> return $
                let v' = DrawOptionNV
                in \k tsm -> generalSurfaceOpt k v' tsm
            SurfaceOptI -> return $
                let v' = DrawOptionI
                in \k v tsm -> generalSurfaceOpt k (v' v) tsm
            SurfaceOptD -> return $
                let v' = DrawOptionD
                in \k v tsm -> generalSurfaceOpt k (v' v) tsm
            SurfaceOptS -> return $
                let v' = DrawOptionS
                in \k v tsm -> generalSurfaceOpt k (v' v) tsm
            SurfaceOptB -> return $
                let v' = DrawOptionB
                in \k v tsm -> generalSurfaceOpt k (v' v) tsm
            SurfaceOptIA -> return $
                let v' = DrawOptionIA
                in \k v tsm -> generalSurfaceOpt k (v' v) tsm
            SurfaceOptDA -> return $
                let v' = DrawOptionDA
                in \k v tsm -> generalSurfaceOpt k (v' v) tsm
            SurfaceOptSA -> return $
                let v' = DrawOptionSA
                in \k v tsm -> generalSurfaceOpt k (v' v) tsm
            SurfaceOptBA -> return $
                let v' = DrawOptionBA
                in \k v tsm -> generalSurfaceOpt k (v' v) tsm

            Map -> askFn mapHO

            When -> return when
            WhenElse -> return whenElse
            WhenTransformed -> return whenTransformed
            WhenElseTransformed -> return whenElseTransformed

            HasAnyValue -> return hasAnyValue

            HasAllValue -> return hasAllValue

            HasNoValue -> return hasNoValue

            HasAnyValueInRange -> return hasAnyValueInRange

            HasAllValueInRange -> return hasAllValueInRange

            HasNoValueInRange -> return hasNoValueInRange


            GreaterThan -> return $ \x y -> let predicate = (> x)
                                              in predicate y
            GreaterThanOrEqual -> return $ \x y -> let predicate = (>= x)
                                                     in predicate y
            LessThan -> return $ \x y -> let predicate = (< x)
                                           in predicate y
            LessThanOrEqual -> return $ \x y -> let predicate = (<= x)
                                                  in predicate y

            GetAvg -> return ("Avg", tscAvg)
            GetMin -> return ("Min", tscMin)
            GetMax -> return ("Max", tscMax)
            GetCount -> return ("Count", fromIntegral . tscCount)

            Not -> return not

            ApplyWithValue -> return applyWithValue
            GetLargest -> return getLargest
            GetSmallest -> return getSmallest
            GetLargestInRange -> return getLargestInRange
            GetSmallestInRange -> return getSmallestInRange

            LargerD -> return max
            SmallerD -> return min
            MulD -> return (*)
            AddD -> return (+)
            DivD -> return (/)
            SubD -> return (-)

            CounterIntermediate _ -> error "This Expression has not been resolved"
            x -> error $ "Missing: " ++ show x
    logExpr ex
    res
    where
        askFn fn = do
            wC <- asks (\World {wConstraints} -> wConstraints )
            return $ fn wC

generalGraphOpt :: TS.Text
                -> DrawOption
                -> ReturnData
                -> ReturnData
generalGraphOpt k v pt@ParadoxReturn { timeSeriesMap = tsm } =
    let appendOps k' v' pts = pts { ptsDrawOptions = ptsDrawOptions pts <> MappedOptions (M.fromList [(k',v')])}
    in pt { timeSeriesMap = M.map (appendOps k v) tsm }

generalSurfaceOpt :: TS.Text
                  -> DrawOption
                  -> ReturnData
                  -> ReturnData
generalSurfaceOpt k v pt@ParadoxReturn { options = opts } =
    let newOpts k' v' = MappedOptions $ M.fromList [(k',v')]
    in pt { options = opts <> newOpts k v }

generalAxisOpt :: TS.Text
               -> TS.Text
               -> DrawOption
               -> ReturnData
               -> ReturnData
generalAxisOpt ax k v pt@ParadoxReturn { axisOptions = axisOpts } =
    let newAxisOpts ax' k' v' = MappedOptions (M.fromList [(ax', DrawOptionR $ MappedOptions $ M.fromList [(k',v')])])
    in pt { axisOptions = axisOpts <> newAxisOpts ax k v }

logExpr :: Expr a
        -> WriterT Accum (Reader World) ()
logExpr ex = do
    let res = \case
            CounterN _  -> LogEntry "counter_name" Lit
            NumD _  -> LogEntry "num_double" Lit
            NumI _  -> LogEntry "num_integer" Lit
            BoolC _  -> LogEntry "bool" Lit
            NumIA _  -> LogEntry "num_integer_array" Lit
            NumDA _  -> LogEntry "num_double_array" Lit
            StrA _  -> LogEntry "string_array" Lit
            BoolCA _  -> LogEntry "bool_array" Lit
            Id -> LogEntry "id" Func
            Sum -> LogEntry "sum" Func
            GraphField -> LogEntry "graph_field" Func
            MaxAtEachSample -> LogEntry "max_at_each_sample" Func
            Summarize _ -> LogEntry "summarize" Func
            Avg -> LogEntry "average" Func
            Abs -> LogEntry "absoluteValue" Func
            MovingAverage -> LogEntry "moving_average" Func
            MedianFilter -> LogEntry "median_filter" Func
            MedianAbsoluteDeviationFilter -> LogEntry "median_absolute_deviation_filter" Func
            MedianAbsoluteDeviation -> LogEntry "median_absolute_deviation" Func
            DoubleMedianAbsoluteDeviationFilter -> LogEntry "double_median_absolute_deviation_filter" Func
            DoubleMedianAbsoluteDeviation -> LogEntry "double_median_absolute_deviation" Func
            Loess -> LogEntry "loess" Func
            EDMMulti -> LogEntry "edm_multi" Func
            Stl -> LogEntry "stl" Func
            StlSeason -> LogEntry "stlSeason" Func
            StlTrend -> LogEntry "stlTrend" Func
            StlResidual -> LogEntry "stlResidual" Func
            RemoveAbovePercentile -> LogEntry "remove_above_percentile" Func
            RemoveAbovePercentileInc -> LogEntry "remove_above_percentile_inclusive" Func
            RemoveBelowPercentile -> LogEntry "remove_below_percentile" Func
            RemoveBelowPercentileInc -> LogEntry "remove_below_percentile_inclusive" Func
            RemoveAboveValue -> LogEntry "remove_above_value" Func
            RemoveAboveValueInc -> LogEntry "remove_above_value_inclusive" Func
            RemoveBelowValue -> LogEntry "remove_below_value" Func
            RemoveBelowValueInc -> LogEntry "remove_below_value_inclusive" Func
            Ceiling -> LogEntry "ceiling" Func
            Floor -> LogEntry "floor" Func
            KeepSeriesAbove -> LogEntry "keep_series_above" Func
            KeepSeriesAboveInc -> LogEntry "keep_series_above_inclusive" Func
            KeepSeriesBelow -> LogEntry "keep_series_below" Func
            KeepSeriesBelowInc -> LogEntry "keep_series_below_inclusive" Func
            KeepSeriesAllAbove -> LogEntry "keep_series_all_above" Func
            KeepSeriesAllAboveInc -> LogEntry "keep_series_all_above_inclusive" Func
            KeepSeriesAllBelow -> LogEntry "keep_series_all_below" Func
            KeepSeriesAllBelowInc -> LogEntry "keep_series_all_below_inclusive" Func
            Offset -> LogEntry "offset" Func
            KeepSeriesAboveDeviation -> LogEntry "keep_series_above_deviation" Func
            KeepSeriesAboveDeviationInc -> LogEntry "keep_series_above_deviation_inclusive" Func
            KeepSeriesWithinDeviation -> LogEntry "keep_series_within_deviation" Func
            KeepSeriesWithinDeviationInc -> LogEntry "keep_series_within_deviation_inclusive" Func
            Sub -> LogEntry "subtract" Func
            Divide -> LogEntry "divide" Func
            Cons -> LogEntry "cons" Func
            Integrate -> LogEntry "integrate" Func
            Derive -> LogEntry "derive" Func
            Log -> LogEntry "log" Func
            Scale -> LogEntry "scale" Func
            ScalarDivide -> LogEntry "scalarDivide" Func
            ScaleByInterval -> LogEntry "scale_by_interval" Func
            DivideByInterval -> LogEntry "divide_by_interval" Func
            Alias -> LogEntry "alias" Func
            SmartAlias -> LogEntry "smart_alias" Func
            SmartAliasA -> LogEntry "smart_alias_array" Func
            Exclude -> LogEntry "exclude" Func
            Include -> LogEntry "include" Func
            ConstantLine -> LogEntry "constant_line" Func
            _ :|: _   -> LogEntry "infix_apply" Bind
            Apply _ _   -> LogEntry "apply" Bind
            _ :.: _   -> LogEntry "compose" Bind
            _ :>>>: _   -> LogEntry "distribute" Bind
            _ :...: _   -> LogEntry "chain" Bind
            Counter _ -> LogEntry "counters" Lit
            GraphOpts -> LogEntry  "graph_options_json" Pass
            GraphOptI -> LogEntry "graph_options_integer" Pass
            GraphOptD -> LogEntry "graph_options_double" Pass
            GraphOptS -> LogEntry "graph_options_string" Pass
            GraphOptB -> LogEntry "graph_options_bool" Pass
            GraphOptIA -> LogEntry "graph_options_integer_array" Pass
            GraphOptDA -> LogEntry "graph_options_double_array" Pass
            GraphOptSA -> LogEntry "graph_options_string_array" Pass
            GraphOptBA -> LogEntry "graph_options_bool_array" Pass
            AxisOpts -> LogEntry  "axis_options_json" Pass
            AxisOptI -> LogEntry "axis_options_integer" Pass
            AxisOptD -> LogEntry "axis_options_double" Pass
            AxisOptS -> LogEntry "axis_options_string" Pass
            AxisOptB -> LogEntry "axis_options_bool" Pass
            AxisOptIA -> LogEntry "axis_options_integer_array" Pass
            AxisOptDA -> LogEntry "axis_options_double_array" Pass
            AxisOptSA -> LogEntry "axis_options_string_array" Pass
            AxisOptBA -> LogEntry "axis_options_bool_array" Pass
            SurfaceOpts -> LogEntry  "surface_options_json" Pass
            SurfaceOpt -> LogEntry "surface_options_novalue" Pass
            SurfaceOptI -> LogEntry "surface_options_integer" Pass
            SurfaceOptD -> LogEntry "surface_options_double" Pass
            SurfaceOptS -> LogEntry "surface_options_string" Pass
            SurfaceOptB -> LogEntry "surface_options_bool" Pass
            SurfaceOptIA -> LogEntry "surface_options_integer_array" Pass
            SurfaceOptDA -> LogEntry "surface_options_double_array" Pass
            SurfaceOptSA -> LogEntry "surface_options_string_array" Pass
            SurfaceOptBA -> LogEntry "surface_options_bool_array" Pass

            Map -> LogEntry "map" HigherOrder

            When -> LogEntry "when" HigherOrder
            WhenElse -> LogEntry "when_else" HigherOrder

            WhenTransformed -> LogEntry "when_transformed" HigherOrder
            WhenElseTransformed -> LogEntry "when_else_transformed" HigherOrder

            HasAnyValue -> LogEntry "has_any_value" HigherOrder
            HasAllValue -> LogEntry "has_all_value" HigherOrder
            HasNoValue -> LogEntry "has_no_value" HigherOrder

            HasAnyValueInRange -> LogEntry "has_any_value_in_range" HigherOrder
            HasAllValueInRange -> LogEntry "has_all_value_in_range" HigherOrder
            HasNoValueInRange -> LogEntry "has_no_value_in_range" HigherOrder

            GreaterThan -> LogEntry "greater_than" HigherOrder
            LessThan -> LogEntry "less_than" HigherOrder
            GreaterThanOrEqual -> LogEntry "greater_than_or_equal" HigherOrder
            LessThanOrEqual -> LogEntry "less_than_or_equal" HigherOrder

            GetAvg -> LogEntry "get_avg" HigherOrder
            GetMin -> LogEntry "get_min" HigherOrder
            GetMax -> LogEntry "get_max" HigherOrder
            GetCount -> LogEntry "get_count" HigherOrder

            Not -> LogEntry "not" HigherOrder

            ApplyWithValue -> LogEntry "apply_with_value" HigherOrder
            GetLargest -> LogEntry "get_largest" HigherOrder
            GetSmallest -> LogEntry "get_smallest" HigherOrder
            GetLargestInRange -> LogEntry "get_largest_in_range" HigherOrder
            GetSmallestInRange -> LogEntry "get_smallest_in_range" HigherOrder

            LargerD -> LogEntry "larger_double" HigherOrder
            SmallerD -> LogEntry "smaller_double" HigherOrder
            MulD -> LogEntry "multiply_double" HigherOrder
            AddD -> LogEntry "add_double" HigherOrder
            DivD -> LogEntry "multiply_double" HigherOrder
            SubD -> LogEntry "add_double" HigherOrder

            x -> error $ "Missing: " ++ show x
    tell Accum { unLogs = [res ex] }
