{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Paradox.Eval
( module Paradox.Eval.Types
, guardOnKeys
, guardOnQueries
, pruneReplies
, fetchEventsForEval
, fetchCountersForEval
, typeCheckQueries
, typeCheckQueriesAndResolve
, eval
, keysFromQueries
, calculateQueryBounds
) where

import            Control.Arrow                  ( (&&&)
                                                 , second
                                                 )
import            Control.Concurrent.STM
import            Control.Monad                  ( forM )
import            Control.Parallel.Strategies    ( parMap
                                                 , rpar
                                                 )
import            Data.Maybe                     ( mapMaybe )
import            Data.Monoid                    ( (<>) )
import            Data.Time.Clock                ( diffUTCTime )
import            Language.Paradox.Eval          ( filterCounters
                                                 , collectCounters
                                                 , resolve
                                                 , padding
                                                 , evalWithEnv
                                                 , Padding (..)
                                                 )
import            Language.Paradox.Eval.Types
import            Language.Paradox.Util          ( typeCheck )
import            Paradox.Debug
import            Paradox.Eval.Types
import            Paradox.Events                 ( getAllEvents )
import            Paradox.Istatd.CounterMap      ( make
                                                 , lookupMagic
                                                 , convertForSearch
                                                 , unpackSearch
                                                 , deMaybe
                                                 )
import            Paradox.Istatd.Functions       ( manyConvTSToPTS )
import            Paradox.Logger                 ( logVerbose
                                                 , Log(..)
                                                 )
import            Paradox.Util                   ( descriptiveHead
                                                 , Partitionable (..)
                                                 )

import qualified  Data.List.NonEmpty             as NonEmpty
import qualified  Data.Map.Strict                as M
import qualified  Data.Set                       as S
import qualified  Data.Text                      as TS
import qualified  Data.Time.Clock.POSIX          as POSIX
import qualified  Data.Time.Lens                 as TL
import qualified  Paradox.Events.Types           as E
import qualified  Paradox.Istatd                 as I
import qualified  Paradox.Istatd.Lifted          as IstatdLifted
import qualified  Paradox.Istatd.Types           as IT
import qualified  Paradox.Types                  as PT

keysFromQueries'
  :: PT.ServerState
  -> [ContextEvaluable (Expr IT.ParadoxReturn)]
  -> IO [IT.CounterSpec]
keysFromQueries' state' typedQueries = do
  cmcCache <- atomically $
    readTMVar $ PT.csCounterMapCache . PT.ssCacheState $ state'
  return $ keysFromQueries cmcCache typedQueries

keysFromQueries
  :: PT.CounterMapCache
  -> [ContextEvaluable (Expr IT.ParadoxReturn)]
  -> [IT.CounterSpec]
keysFromQueries (PT.CounterMapCache csMap _ _) typedQueries =
  let
    keys = S.toList
         $ S.fromList
         $ concatMap getCtrs typedQueries

    getCtrs query =
      let
        ctrs = collectCounters (cevQ query)
        keys'' = map fst $ concatMap performLookup ctrs
      in
        filterCounters (cevQ query) keys''

    performLookup
      :: IT.CounterSpec
      -> [(IT.CounterSpec, IT.CountersMatch)]
    performLookup cs =
      let
        removeOffset
          :: IT.CounterSpec
          -> IT.CounterSpec
        removeOffset c = c { IT.csOffset = Nothing }

        find
          :: IT.CounterSpec
          -> [(IT.CounterSpec, IT.CountersMatch)]
        find = unpackSearch
             . deMaybe
             . (`lookupMagic` csMap)
             . convertForSearch
             . removeOffset

        coerce
          :: [(IT.CounterSpec, IT.CountersMatch)]
          -> [(IT.CounterSpec, IT.CountersMatch)]
        coerce = fmap (\(x,y) -> (x { IT.csOffset = IT.csOffset cs}, y))
      in
        coerce . find $ cs
  in
    keys


paddingToSeconds
  :: Padding
  -> Int
  -> Int
paddingToSeconds (Buckets x) s = s * x
paddingToSeconds (TimeSpan x) _ = x

calculateQueryBounds
  :: IT.ParadoxQuery
  -> [ContextEvaluable (Expr IT.ParadoxReturn)]
  -> QueryBounds
calculateQueryBounds pq typedQueries =
  let
    timeRange = IT.pqRange pq
    maxSamples = IT.pqMaxSamples pq
    timeDiff = diffUTCTime (IT.unTimeWrapper $ IT.trStop timeRange)
                           (IT.unTimeWrapper $ IT.trStart timeRange)
    sampleSize = floor
               $ IstatdLifted.reduction
               $ maximum [10, realToFrac timeDiff / fromIntegral maxSamples]

    paddingS
      :: [(Int, Int)]
    paddingS = map (\(l,r) -> (paddingToSeconds l sampleSize, paddingToSeconds r sampleSize))
             $ (Buckets 0, Buckets 0):concatMap (padding . cevQ) typedQueries --TODO hack for emptylists

    maxOfPadding
      :: forall a
       . ( Ord a
         , Num a
         )
      => ((Int, Int) -> Int)
      -> a
    maxOfPadding fn = maximum (map (fromIntegral . fn) paddingS)

    maxOfFirsts
      :: forall a
       . ( Ord a
         , Num a
         )
      => a
    maxOfFirsts = maxOfPadding fst

    maxOfSeconds
      :: forall a
       . ( Ord a
         , Num a
         )
      => a
    maxOfSeconds = maxOfPadding snd

    paddingL
      :: Int
    paddingL = maxOfFirsts

    paddingR
      :: Int
    paddingR = maxOfSeconds

    timeRange'
      = timeRange
      { IT.trStart
          = IT.TimeWrapper
          $ TL.modL TL.seconds
                    (subtract $ fromIntegral paddingL)
                    (IT.unTimeWrapper $ IT.trStart timeRange)
      , IT.trStop
          = IT.TimeWrapper
          $ TL.modL TL.seconds
                    ((+) $ fromIntegral paddingR)
                    (IT.unTimeWrapper $ IT.trStop timeRange)
      }

    maxSamples'
      = maxSamples
      + (maxOfFirsts `div` sampleSize)
      + (maxOfSeconds `div` sampleSize)

  in
    QueryBounds
    { qbOrigTimeRange = timeRange
    , qbTimeRange = timeRange'
    , qbOrigMaxSamples = maxSamples
    , qbMaxSamples = maxSamples'
    }

eval
  :: [ContextEvaluable (Expr IT.ParadoxReturn)]
  -> [(Maybe Int, IT.PostReplyN)]
  -> M.Map TS.Text [E.EventData]
  -> QueryBounds
  -> EvalStatus
eval typedQueries replies eventData qb =
  let
    res' = map (second IT.prnKeys) replies

    env
      = World
      { wMap = make $ manyConvTSToPTS res'
      , wConstraints
          = WorldConstraints
          { wMaxSamples = qbMaxSamples qb
          , wTimeRange = qbOrigTimeRange qb
          , wInterval = Just interval
          }
      }

    interval
      = IT.prnInterval
      $ snd
      $ descriptiveHead "Couldnt get an interval from empty list of replies in eval"
                         replies

    queryResults' = parMap rpar (cevQS &&& (evalW . cevQ)) typedQueries

    (queryResults, analyeResults) = analyzeEval queryResults'

    evalW = evalWithEnv env

    normRange = IstatdLifted.normalizeRange (qbOrigTimeRange qb) interval

    mkReply
      = IT.ParadoxReply normRange
                        interval
                        (pruneReplies normRange queryResults)
                        eventData

  in
    EvalSuccess mkReply analyeResults

analyzeEval
  :: [(TS.Text,(IT.ParadoxReturn, Accum))]
  -> (M.Map TS.Text IT.ParadoxReturn, EvalResult)
analyzeEval queryResults' =
  let
    queryResults
      = M.fromList
      $ map (\(n, (r, _)) -> (n, r)) queryResults'

    numQueries = length queryResults'

    accums = map (\(_, (_, l)) -> l) queryResults'

    accumLogs = foldr1 (<>) accums

    getExprType e = filter ((==) e . exprType)

    countExprType e ls = length $ getExprType e ls

    perQuery logs = map (`countExprType` logs) [minBound..maxBound]

    counts = map (\Accum {..} -> perQuery unLogs) accums

    sAvg x = fromIntegral x / fromIntegral (length counts)

    avgT (f, b, l ,p, h)
      = (sAvg f, sAvg b, sAvg l, sAvg p, sAvg h)

    (funcPerQuery, bindsPerQuery, litsPerQuery, passesPerQuery, higherOrderPerQuery)
      = avgT
      $ foldr1 (\(f, b, l, p, h) (af, ab, al, ap, ah) ->
                 (f + af, b + ab, l + al, p + ap, h + ah)
               )
               $ mapMaybe convertToTuple counts

    convertToTuple it = case it of
      [f,b,l,p,h] -> Just (f,b,l,p,h)
      _ -> Nothing

    exprPartsPerQuery
      = funcPerQuery
      + bindsPerQuery
      + litsPerQuery
      + passesPerQuery
      + higherOrderPerQuery

    evalResults = EvalResult {..}

  in
    (queryResults, evalResults)

typeCheckQueries
  :: IT.ParadoxQuery
  -> QueryStatus
typeCheckQueries pq =
  let
    queries = IT.pqKeys pq

    (failedQueries, queries') = partition queries

    (failedTypedQueries, typedQueries)
      = partition
      $ map (\x ->
              CEvaluable (IT.ceQS x) (typeCheck . IT.ceQ $ x)
            )
            queries'

    res = case (failedQueries, failedTypedQueries) of
      ([],[]) -> QueryUnresolvedSuccess typedQueries
      ([], x) -> QueryMisTypedQueries "Queries did not type check" x
      (x, _)  -> QueryMalformedSyntax "Bad query syntax" x

  in
    res

-- See https://github.com/ndmitchell/hlint/issues/180
{-# ANN typeCheckQueriesAndResolve ("HLint: ignore Move brackets to avoid $" :: String) #-}
typeCheckQueriesAndResolve
  :: PT.ServerState
  -> IT.ParadoxQuery
  -> IO QueryStatus
typeCheckQueriesAndResolve state' pq = do
  let
    typedQueriesStatus = typeCheckQueries pq
    conf = PT.ssConfig state'
  case typedQueriesStatus of
    QueryUnresolvedSuccess typedQueries -> do
      (failedResolvedQueries, finalQueries) <- partition <$>
        ( forM typedQueries $ \x -> do
            logVerbose conf $ LogText
                            $ "Resolving query"
                           <> TS.pack (show x)

            resolvedQuery <- resolve (PT.getSandboxedCommands $ PT.ssConfig state') $ cevQ x

            logVerbose conf $ LogText
                            $ "Resolved query"
                           <> TS.pack (show x)

            return $ x { cevQ = resolvedQuery }
        )
      let
        res = case failedResolvedQueries of
          [] -> QuerySuccess finalQueries
          x  -> QueryUnresolveableSandboxActions "Couldnt resolve shell commands" x
      return res
    x -> return x

fetchEventsForEval
  :: PT.ServerState
  -> [IT.EventReq]
  -> QueryBounds
  -> IO (M.Map TS.Text [E.EventData])
fetchEventsForEval state' events qb = do
  let
    conf = PT.ssConfig state'
    eventConfig = PT.getEventConfig conf
  case eventConfig of
    Just evConfig ->
      getAllEvents (PT.ssManager state') evConfig events (qbTimeRange qb)
    Nothing -> return mempty

fetchCountersForEval
  :: PT.ServerState
  -> [ContextEvaluable (Expr IT.ParadoxReturn)]
  -> QueryBounds
  -> IO KeysStatus
fetchCountersForEval state' typedQueries qb = do
  keys <- keysFromQueries' state' typedQueries
  timeIt "TotalTime getting batch of counters" $
    I.batchGetStatsViaCacheCompactCE'
      (PT.ssManager state')
      (PT.ssConfig state')
      (PT.ssCache state')
      keys
      (qbTimeRange qb)
      (qbMaxSamples qb)

guardOnQueries
  :: QueryStatus
  -> Maybe [ContextEvaluable (Expr IT.ParadoxReturn)]
guardOnQueries (QuerySuccess v) = Just v
guardOnQueries _ = Nothing

guardOnKeys
  :: KeysStatus
  -> Maybe [(Maybe Int, IT.PostReplyN)]
guardOnKeys (KeysSuccess v) = Just v
guardOnKeys _ = Nothing

pruneReplies
  :: IT.TimeRange
  -> M.Map TS.Text IT.ParadoxReturn
  -> M.Map TS.Text IT.ParadoxReturn
pruneReplies normRange orig =
  let
    reduce pts = do
      let
        ptsData = IT.ptsData pts
      ptsData' <- NonEmpty.nonEmpty $
        NonEmpty.filter
          (\x ->
            let
              timeToStartInteger
                = floor
                . POSIX.utcTimeToPOSIXSeconds
                . IT.unTimeWrapper
                . IT.trStart
            in
              IT.tscTime x >= timeToStartInteger normRange
          )
          ptsData
      return $ pts { IT.ptsData = ptsData' }

  in
    M.map
      (\pt@IT.ParadoxReturn { IT.timeSeriesMap = tsm } ->
        pt { IT.timeSeriesMap = M.mapMaybe reduce tsm }
      )
      orig
