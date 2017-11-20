{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Paradox.Istatd.Functions (
  module Paradox.Istatd.Types
, zeroTSC
, rekey
, magic
, compute
, manyConvTSToPTS
, convTSToPTS
, inSameTimeTS
, inSameTimePTS
, mkEmptyTimeSeriesFromAvg
, fromPostRequestToCompact
, toEnd
, toStop
, toCounterName
, timeSpan
, combinePostReplies
, finalizeIntermediateCounterSpec
, correctWithOffset
, correctTimeSeriesIntervals
, correctTimeSeriesWithOffset
, correctTimeSeriesChunkWithOffset
, shift
, convertToSafeNameInPartsMany
, convertToSafeNameInParts
, convertToSafeParts
, correctIntervals
, createPostRequest
, shard
, unshard
, mkTimeRange
, removeEmpties
) where

import Prelude                                      hiding (concatMap)

import Data.Default

import Paradox.Istatd.Types
import Paradox.Debug

import Data.List.NonEmpty                           (NonEmpty)
import Control.Applicative                          ((<|>))
import Control.Monad                                (join)
import Data.Either                                  ( lefts
                                                    , rights
                                                    )
import Paradox.Util                                 ( safeHead
                                                    , descriptiveHead
                                                    , foldl1'
                                                    )

import Data.Time.Clock                              (addUTCTime)
import Data.Maybe                                   ( catMaybes
                                                    , fromJust
                                                    )
import Data.List.Split                              (chunksOf)
import Data.Foldable                                ( concatMap
                                                    , foldl'
                                                    )

import qualified Data.Time.Clock.POSIX              as POSIX
import qualified Data.Map.Strict                    as M
import qualified Data.Text                          as TS
import qualified Data.List.NonEmpty                 as NonEmpty


zeroTSC :: TimeSeriesChunk
        -> TimeSeriesChunk
zeroTSC tsc = def { tscTime = tscTime tsc }

correctWithOffset :: Maybe Int
                  -> PostReply
                  -> PostReply
correctWithOffset offset pr =
    case offset of
        Nothing -> pr
        Just offset' -> pr { prKeys = M.map (correctTimeSeriesWithOffset offset') $ prKeys pr }

correctTimeSeriesWithOffset :: Int
                            -> TimeSeries
                            -> TimeSeries
correctTimeSeriesWithOffset offset ts = ts {
    tsData = map (correctTimeSeriesChunkWithOffset offset) $ tsData ts
    }

correctTimeSeriesChunkWithOffset :: Int
                                 -> TimeSeriesChunk
                                 -> TimeSeriesChunk
correctTimeSeriesChunkWithOffset offset tsc@TimeSeriesChunk {..} =
    case tscTime of
        0 -> tsc
        t -> tsc { tscTime = t - fromIntegral offset }


correctIntervals :: [(a, PostReplyN)]
                 -> [(a, PostReplyN)]
correctIntervals prs =
    let intervalList = concatMap (M.elems . M.map tsnInterval . prnKeys . snd) prs
        firstElem minInterval = join $ (fmap NonEmpty.head . NonEmpty.nonEmpty . NonEmpty.dropWhile ((==0) . tscTime)) <$> findElem minInterval
        findElem :: Int
                 -> Maybe (NonEmpty TimeSeriesChunk)
        findElem minInterval = NonEmpty.nonEmpty $ concatMap (concatMap (NonEmpty.toList . snd)
                . catMaybes
                . M.elems
                . M.mapWithKey (\k x ->
                    if tsnInterval x == minInterval
                        then Just (k, tsnData x)
                        else Nothing
                    )
                . prnKeys
                . snd)
                   prs
        mutate n (a, pr@PostReplyN {..}) = (a, pr { prnInterval = n, prnKeys = newKeys n prnKeys })
        newKeys n = M.mapMaybe (correctTimeSeriesIntervals n (firstElem n))
    in case intervalList of
         [] -> prs
         intervalList' -> let minInterval = minimum intervalList'
                          in map (mutate minInterval) prs

correctTimeSeriesIntervals :: Int
                           -> Maybe TimeSeriesChunk
                           -> TimeSeriesN
                           -> Maybe TimeSeriesN
correctTimeSeriesIntervals _ Nothing ts@TimeSeriesN {..} = Just ts
correctTimeSeriesIntervals i (Just f) ts@TimeSeriesN {..} = case NonEmpty.length tsnData of
    1 -> Just ts
    _ -> correctTimeSeriesIntervals' i f ts

correctTimeSeriesIntervals' :: Int
                            -> TimeSeriesChunk
                            -> TimeSeriesN
                            -> Maybe TimeSeriesN
correctTimeSeriesIntervals' newInterval firstElem ts@TimeSeriesN {..} = do
    let newRange =
            let end = floor (POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper $ treEnd tsnRange)
                start = floor (POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper $ treStart tsnRange)
                diff :: Int
                diff = end - start
                num :: Double
                num = fromIntegral diff / fromIntegral newInterval
                num' = round (num + 0.5)
            in num'
        ratio :: Int
              -> Double
        ratio x =
            let xD,iD :: Double
                xD = fromIntegral x
                iD = fromIntegral tsnInterval
                iD' :: Integer
                iD' = floor (xD / iD)
            in (xD - (fromIntegral iD' * iD )) / iD
        indexes = NonEmpty.nonEmpty $ map ((\x -> ( x `div` tsnInterval, ratio x)) . (newInterval *)) [0..newRange]
        pairs = NonEmpty.nonEmpty $ zip (NonEmpty.toList tsnData) (NonEmpty.tail tsnData)

        newData = magic pairs indexes
    newData' <- do
            ourFirst <- (tscTime . NonEmpty.head) <$> newData
            let theirFirst = tscTime firstElem
                offset = fromIntegral ((fromIntegral ourFirst - fromIntegral theirFirst) `mod` newInterval)
            offsetData <- NonEmpty.map (\tsc -> tsc { tscTime = tscTime tsc - offset}) <$> newData
            return $ rekey offsetData newInterval
    if newInterval == tsnInterval
        then do
            ourFirst <- safeHead $ NonEmpty.dropWhile ((==0) . tscTime) tsnData
            let theirFirst = traceIt "Their first elem " $ tscTime firstElem
            return $
                        let offset = ourFirstT - theirFirst
                            ourFirstT = traceIt "Our first elem " $ tscTime ourFirst
                        in case (ourFirstT `compare` theirFirst, (theirFirst - ourFirstT) `mod` fromIntegral newInterval) of
                            (EQ, 0) -> ts
                            (_, 0) -> ts
                            (d,_) -> case shift d offset tsnInterval ourFirst (traceIt "WHAT IS THIS DATA" tsnData) of
                                Just dat -> ts { tsnData = dat }
                                Nothing -> ts

        else Just $ ts { tsnInterval = newInterval, tsnData = newData' }

shift :: Ordering
      -> Integer
      -> Int
      -> TimeSeriesChunk
      -> NonEmpty TimeSeriesChunk
      -> Maybe (NonEmpty TimeSeriesChunk)
shift d offset interval firstElem tsData =
    let pairs = case d of
            GT -> (firstElem, firstElem):zip (NonEmpty.tail tsData) (NonEmpty.toList tsData)
            _ -> zip (NonEmpty.toList tsData) (NonEmpty.tail tsData)
        fixOffset = case d of
            GT -> offset
            _ -> negate offset
        remake tsc n =
            let useTime = case d of
                    GT -> tscTime tsc
                    _ -> tscTime tsc
            in compute
                (tsc { tscTime = useTime - offset }, n)
                (fromIntegral fixOffset / fromIntegral interval)
    in NonEmpty.nonEmpty $ map (uncurry remake) pairs

rekey :: NonEmpty TimeSeriesChunk
      -> Int
      -> NonEmpty TimeSeriesChunk
rekey tsd interval =
    let startTime = (fromIntegral . tscTime) $ NonEmpty.head tsd
        newTimes startTime' = NonEmpty.fromList [startTime',(startTime' + interval)..]
    in NonEmpty.zipWith (\x y -> x { tscTime = fromIntegral y} ) tsd (newTimes startTime)

magic :: Maybe (NonEmpty (TimeSeriesChunk,TimeSeriesChunk))
      -> Maybe (NonEmpty (Int, Double))
      -> Maybe (NonEmpty TimeSeriesChunk)
magic Nothing _ = Nothing
magic _ Nothing = Nothing
magic (Just pairs) (Just indexes) = magic' pairs indexes 0

magic' :: NonEmpty (TimeSeriesChunk,TimeSeriesChunk)
       -> NonEmpty (Int, Double)
       -> Int
       -> Maybe (NonEmpty TimeSeriesChunk)
magic' pairs'' ixs'' cur'' = NonEmpty.nonEmpty $ magic'' (NonEmpty.toList pairs'') (NonEmpty.toList ixs'') cur''
    where
        magic'' [] _ _ = []
        magic'' ((x,_):_) [] _ = [x]
        magic'' pairs@(x:xs) ((idx, rat):ixs) cur = if idx == cur
            then compute x rat : magic'' pairs ixs idx
            else case xs of
                [] -> []
                (y:_) -> compute y rat : magic'' xs ixs idx

compute :: (TimeSeriesChunk, TimeSeriesChunk)
        -> Double
        -> TimeSeriesChunk
compute (tsl, tsr) rat =
    let avgDiff = tscAvg tsr - tscAvg tsl
        sumDiff = tscSum tsr - tscSum tsl
        sumsqDiff = tscSumsq tsr - tscSumsq tsl
        minDiff = tscMin tsr - tscMin tsl
        maxDiff = tscMax tsr - tscMax tsl
        countDiff = tscCount tsr - tscCount tsl
        sdevDiff = tscSdev tsr - tscSdev tsl
        avgStep = avgDiff * rat
        sumStep = sumDiff * rat
        sumsqStep = sumsqDiff * rat
        minStep = minDiff * rat
        maxStep = maxDiff * rat
        countStep = floor (fromIntegral countDiff * rat)
        sdevStep = sdevDiff * rat
    in tsl { tscAvg = tscAvg tsl + avgStep
           , tscSum = tscSum tsl + sumStep
           , tscSumsq = tscSumsq tsl + sumsqStep
           , tscMin = tscMin tsl + minStep
           , tscMax = tscMax tsl + maxStep
           , tscCount = tscCount tsl + countStep
           , tscSdev = tscSdev tsl + sdevStep
           }

convTSToPTS :: (Maybe Int, M.Map CounterName TimeSeriesN)
            -> TSMap
convTSToPTS (offset, old) = M.fromList $
    map (\(k, TimeSeriesN r i d) ->
        (CounterSpec k offset, ParadoxTimeSeries r i d (TS.pack $ show $ CounterSpec k offset) mempty)) $
        M.toList old

manyConvTSToPTS :: [(Maybe Int, M.Map CounterName TimeSeriesN)]
                -> TSMap
manyConvTSToPTS old = foldl1' M.union $ map convTSToPTS old

inSameTimeTS :: TimeSeries
             -> TimeSeries
             -> Bool
inSameTimeTS (TimeSeries rL iL _) (TimeSeries rR iR _) = rL == rR && iL == iR

--Maybe should check lengths of time are the same? or same number of
--buckets??
--inSameTimePTS :: ParadoxTimeSeries -> ParadoxTimeSeries -> Bool
--inSameTimePTS (ParadoxTimeSeries rL iL _ _) (ParadoxTimeSeries rR iR _ _) = rL == rR && iL == iR
inSameTimePTS :: ParadoxTimeSeries
              -> ParadoxTimeSeries
              -> Bool
inSameTimePTS (ParadoxTimeSeries _ iL _ _ _) (ParadoxTimeSeries _ iR _ _ _) = iL == iR


--hack for now...
mkEmptyTimeSeriesFromAvg :: (Integer, Double)
                         -> TimeSeriesChunk
mkEmptyTimeSeriesFromAvg (time, avg) = TimeSeriesChunk time 0 0.0 0.0 0.0 0.0 avg 0.0

fromPostRequestToCompact :: PostRequest
                         -> Bool
                         -> PostRequestCompact
fromPostRequestToCompact PostRequest {..} v = PostRequestCompact {
                                          preqcRange = preqRange
                                        , preqcKeys = preqKeys
                                        , preqcMaxSamples = preqMaxSamples
                                        , preqcCompact = v
                                     }

toEnd :: TimeRange
      -> TimeRangeE
toEnd TimeRange {..} = TimeRangeE {..}
    where
        treStart = trStart
        treEnd = trStop

toStop :: TimeRangeE
       -> TimeRange
toStop TimeRangeE {..} = TimeRange {..}
    where
        trStart = treStart
        trStop = treEnd

mkTimeRange :: (Integral a)
            => a
            -> a
            -> TimeRange
mkTimeRange s e = let toT = TimeWrapper . POSIX.posixSecondsToUTCTime . fromIntegral
                  in TimeRange (toT s) (toT e)

convertToSafeNameInPartsMany :: [CounterNameInParts]
                             -> Either ErrorWrapper [SafeCounterNameInParts]
convertToSafeNameInPartsMany xs =
    let converted = map convertToSafeNameInParts xs
        anyFailed = lefts converted
    in case anyFailed of
        [] -> Right $ rights converted
        fails -> Left $ MkErrorWrapper (TS.concat ["Couldn't convert ", TS.pack $ show xs, "to safe command, these names failed"]) fails

convertToSafeNameInParts :: CounterNameInParts
                         -> Either ErrorWrapper SafeCounterNameInParts
convertToSafeNameInParts x =
    let converted = map convertToSafeParts x
        anyFailed = lefts converted
    in case anyFailed of
        [] -> Right $ rights converted
        fails -> Left $ MkErrorWrapper (TS.concat ["Couldn't convert ", TS.pack $ show x, " to safe command, these parts failed"]) fails




convertToSafeParts :: CounterNameParts
                   -> Either ErrorWrapper SafeCounterNameParts
convertToSafeParts = \case
    CNPString x -> Right $ SCNPString x
    CNPGlobStar -> Right SCNPGlobStar
    CNPGlobQuestion -> Right SCNPGlobQuestion
    CNPDot -> Right SCNPDot
    CNPShellReplaced x -> Right $ SCNPShellReplaced x
    part -> Left $ MkErrorWrapper "Couldn't convert to safe command" (show part)

safePartsToText :: SafeCounterNameParts
                -> TS.Text
safePartsToText = \case
    SCNPString x -> TS.toLower x
    SCNPGlobStar -> "*"
    SCNPGlobQuestion -> "?"
    SCNPDot -> "."
    SCNPShellReplaced x -> TS.toLower x

toCounterName :: SafeCounterNameInParts
              -> CounterName
toCounterName = TS.concat . map safePartsToText

finalizeIntermediateCounterSpec :: CounterSpecIntermediate
                                -> [SafeCounterNameInParts]
                                -> [CounterSpec]
finalizeIntermediateCounterSpec CounterSpecIntermediate {..} names =
    let counterNames = map toCounterName names
        cspec = map (\name -> CounterSpec { csName = name, csOffset = csiOffset }) counterNames
    in cspec

timeSpan :: TimeRange
         -> Int
timeSpan TimeRange {..} = let s = floor $ POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper trStart
                              e = floor $ POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper trStop
                          in e - s

combinePostReplies :: [PostReply]
                   -> PostReply
combinePostReplies reps = PostReply range interval comb
    where
        hd = descriptiveHead "Combine post replies fails to get head" reps
        range = prRange hd
        interval = prInterval hd
        maps = map prKeys reps
        comb = M.unions maps


startOf5MinBuckets :: (Show s, Num s)
                   => s
                   -> s
startOf5MinBuckets now = let dInS = (60*60*24)
                         in traceIt "5Min buckets start at " (now - (10 * dInS))

startOf1HBuckets :: (Show s, Num s)
                 => s
                 -> s
startOf1HBuckets now = let dInS = (60*60*24)
                           yInS = dInS * 365
                       in traceIt "1h buckets start at" (now - (yInS + (9 * dInS)))

computeBestBucketPadding :: POSIX.POSIXTime
                         -> Int
                         -> TimeRange
                         -> Int
computeBestBucketPadding now offset tr =
    let ourStartTime = traceIt "we start at " $ POSIX.utcTimeToPOSIXSeconds (unTimeWrapper $ trStart tr) + fromIntegral offset
        bucket
            | ourStartTime < startOf1HBuckets now = 3600
            | ourStartTime > startOf1HBuckets now && ourStartTime < startOf5MinBuckets now = 300
            | otherwise = 0
    in bucket

computeOffsetTimeRange :: POSIX.POSIXTime
                       -> Int
                       -> TimeRange
                       -> TimeRange
computeOffsetTimeRange now offset tr =
    let padding = computeBestBucketPadding now offset tr
    in TimeRange (TimeWrapper $ addUTCTime (fromIntegral offset) $ unTimeWrapper $ trStart tr)
                 (TimeWrapper $ addUTCTime (fromIntegral (offset + padding) ) $ unTimeWrapper $ trStop tr)

createPostRequest :: [CounterSpec]
                  -> TimeRange
                  -> Int
                  -> IO PostRequest
createPostRequest keys' range samples = do
        now <- POSIX.getPOSIXTime
        let preqRange = preqRange' now
        return PostRequest {..}
    where
        preqKeys       = map csName keys'
        preqRange' now = case csOffset $ descriptiveHead "failed to create post request, couldnt get first elem" keys' of
                                Nothing -> range
                                Just offset -> computeOffsetTimeRange now offset range
        preqMaxSamples = samples


shard :: Maybe Int
      -> M.Map (Maybe Int) [CounterSpec]
      -> M.Map (Maybe Int, Int) [CounterSpec]
shard s m =
    let t = M.foldlWithKey' (\acc k v ->
                let mList =
                        if length v > limit
                                then chunksOf limit v
                                else [v]
                    limit = fromJust $ s <|> Just 20
                    l = length mList
                    n = zip (zip (replicate l k) [1..]) mList
                in foldl' (flip (:)) acc n
            ) [] m
    in M.fromList t

unshard :: M.Map (Maybe Int, Int) PostReplyRet
        -> M.Map (Maybe Int) [PostReplyRet]
unshard m =
    let t = M.foldlWithKey' (\acc (k, _) v ->
                (k, [v]):acc
            ) [] m
    in M.fromListWith (++) t

removeEmpties :: PostReply
              -> PostReplyN
removeEmpties PostReply {..} =
    let noEmpties = M.mapMaybe transform prKeys
        transform TimeSeries {..} = do
            tsnData <- NonEmpty.nonEmpty tsData
            return TimeSeriesN { tsnRange = tsRange
                               , tsnInterval = tsInterval
                               , tsnData = tsnData
                               }
    in PostReplyN { prnRange = prRange
                  , prnInterval = prInterval
                  , prnKeys = noEmpties
                  }
