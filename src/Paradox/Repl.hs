{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Paradox.Repl where

import Control.Monad.IO.Class
import Data.Default
import Control.Monad.State.Strict
import Control.Concurrent.MVar
import Data.List (stripPrefix)
import Data.Aeson (encode)
import Paradox.Istatd.Types (TimeWrapper(..), TimeRange(..), ParadoxQuery(..), textToCExpr, fromIntegralI, ParadoxReply(..), ParadoxReturn(..), ParadoxTimeSeries(..), TimeSeriesChunk(..), ContextExpr(..), CounterSpec(..))
import Language.Paradox.Util (typeCheck')
import Paradox.Eval.Types (EvalStatus(..))
import Paradox.Handlers.Istatd (evalShared)
import Data.Foldable (minimumBy, maximumBy)
import System.Console.Haskeline (InputT, runInputTBehavior, useFileHandle, Behavior, defaultBehavior, defaultSettings, getInputLine, outputStrLn)
import System.Console.Haskeline.MonadException (MonadException)
import Paradox.Types (ServerState)
import Language.Paradox.Grammar (Expr, ParadoxParseError)
import System.IO (Handle, hPutStrLn)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as M
import qualified Data.Text                  as TS
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Time.Clock.POSIX          as POSIX
import Data.Char

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight "" = ""
trimRight str | all isSpace str = ""
trimRight (c : cs) = c : trimRight cs

trim :: String -> String
trim = trimLeft . trimRight

process :: MonadIO m
        => MVar ServerState
        -> (String -> ReplT ReplState m ())
        -> String
        -> ReplT ReplState m ()
process st o = go
    where
        go (stripPrefix ":help" -> Just "") = do
            o ":start {timestamp:Integer}"
            o ":end {timestamp:Integer}"
            o ":startNow {offsetFromNow:Integer}"
            o ":endNow {offsetFromNow:Integer}"
            o ":samples {maxBucketSamples:Integer}"
            o ":render {avgs|bars|json}"
            o ":state"
            o ":grammar {query:String}"
            o ":type {query:String}"
            o "Query"
        go (stripPrefix ":start " -> Just rest) = do
            let newStart = read $ trim rest
            modify (\s@ReplState {..} -> s { rsStart = SetTime newStart} )
            o $ "Set start time to : " ++ show newStart
        go (stripPrefix ":end " -> Just rest) = do
            let newEnd = read $ trim rest
            modify (\s@ReplState {..} -> s { rsEnd = SetTime newEnd} )
            o $ "Set end time to : " ++ show newEnd
        go (stripPrefix ":startNow " -> Just rest) = do
            let offset = read $ trim rest
            modify (\s@ReplState {..} -> s { rsStart = Waiting offset $ floor <$> POSIX.getPOSIXTime} )
            o $ "Set start time to now, offset by : " ++ show offset
        go (stripPrefix ":endNow " -> Just rest) = do
            let offset = read $ trim rest
            modify (\s@ReplState {..} -> s { rsEnd = Waiting offset $ floor <$> POSIX.getPOSIXTime} )
            o $ "Set end time to now, offset by : " ++ show offset
        go (stripPrefix ":samples " -> Just rest) = do
            let samples = read $ trim rest
            modify (\s@ReplState {..} -> s { rsSamples = samples} )
            o $ "Set samples to : " ++ show samples
        go (stripPrefix ":state" -> Just "") = do
            stateR <- get
            o $ "REPL state : " ++ show stateR
        go (stripPrefix ":render " -> Just rest) = do
            modify (\s@ReplState {..} -> s { rsRender = trim rest} )
            o $ "Set render to : " ++ show rest
        go (stripPrefix ":grammar " -> Just rest) = do
            let expression = textToCExpr $ TS.pack $ trim rest
            case ceQ expression of
              Left e -> o $ "Error of expression : " ++ show e
              Right g -> o $ "Grammar of expression : " ++ show g
        go (stripPrefix ":type " -> Just rest) = do
            let expression = textToCExpr $ TS.pack $ trim rest
            case ceQ expression of
              Left e -> o $ "Grammar Error of expression : " ++ show e
              Right g -> let ex = typeCheck' g
                         in case ex of
                              Left te -> o $ "TypeCheck error of expression : " ++ show te
                              Right tex -> o $ "Type of expression : " ++ show tex
        go input = do
            let expression = textToCExpr $ TS.pack input
            s <- computeTime <$> gets rsStart
            e <- computeTime <$> gets rsEnd
            s' <- liftIO s
            e' <- liftIO e
            sa <- gets rsSamples
            re <- gets rsRender
        --    s <- getInputLine "Paradox> Start? "
        --    e <- getInputLine "Paradox> End? "
        --    sa <- getInputLine "Paradox> Samples? "
        --    re <- getInputLine "Paradox> Render(avgs,bars,json)? "
            innerProcess st o expression s' e' sa re

innerProcess :: (MonadIO m)
             => MVar ServerState
             -> (String -> m ())
             -> ContextExpr (Either ParadoxParseError Expr)
             -> Integer
             -> Integer
             -> Integer
             -> String
             -> m ()
innerProcess st o expression s' e' sa' re' = do
          let start = TimeWrapper . POSIX.posixSecondsToUTCTime . fromIntegralI $ s'
              end = TimeWrapper . POSIX.posixSecondsToUTCTime . fromIntegralI $ e'
              range = TimeRange start end
          innerState <- liftIO $ readMVar st
          evald <- liftIO $ evalShared (ParadoxQuery range [expression] [] $ fromIntegral sa') innerState
          case evald of
            EvalFailed _ -> return ()
            EvalSuccess res _ -> render o re' res

render :: MonadIO m
       => (String -> m ())
       -> String
       -> ParadoxReply
       -> m ()
render o "json" = o . BSLC.unpack . encode
render o "avgs" = let
                      extractData tsc = let (allAvg, allMin, allMax) = foldr (\TimeSeriesChunk{..} (avg,minv,maxv) -> (avg + tscAvg, minv + tscMin, maxv + tscMax) ) (0,0,0) tsc
                                            len = fromIntegral $ NonEmpty.length tsc
                                        in M.fromList [("avg" :: TS.Text, allAvg / len),("min", allMin / len), ("max", allMax / len)]
                  in o . BSLC.unpack . encode . flip inReply extractData
render o "bars" = let
                      extractData tsc = let remap = NonEmpty.toList $ NonEmpty.map (\TimeSeriesChunk{..}-> getBar min' max' tscAvg ) tsc
                                            min' = tscMin $ minimumBy (\l r -> tscMin l `compare` tscMin r) tsc
                                            max' = tscMax $ maximumBy (\l r -> tscMax l `compare` tscMax r) tsc
                                        in remap
                      barChars :: String
                      barChars = " ▁▂▃▄▅▆▇█"
                      getBar :: Double -> Double -> Double -> Char
                      getBar min' max' n | min' == max' = barChars !! round (fromIntegral (length barChars) / 2 :: Double)
                                         | otherwise    =
                          let len = fromIntegral $ length barChars
                              wid = (max' - min') / (len - 1)
                              idx = round $ (n - min') / wid
                          in barChars !! idx
                  in \rep -> do
                    let forMMap m f = forM_ (M.toList m) $ uncurry f
                    forMMap (inReply rep extractData) $ \q inner -> do
                      o $ "Query: " ++ TS.unpack q
                      forMMap inner $ \k inner2 -> do
                        o $ "Key: " ++ show k
                        o inner2

render _ _ = error "AHAH"

inReply :: ParadoxReply
  -> (NonEmpty.NonEmpty TimeSeriesChunk -> t)
  -> M.Map TS.Text (M.Map CounterSpec t)
inReply ParadoxReply {pprQueries} extractData = M.map inQueries pprQueries
  where
    inQueries ParadoxReturn {timeSeriesMap} = M.map inTimeSeries timeSeriesMap
    inTimeSeries ParadoxTimeSeries {ptsData} = extractData ptsData

--loop :: ( MonadException m
--        , MonadState ReplState (InputT m)
--        )
--     => MVar ServerState
--     -> InputT m ()
loop :: MonadException m => MVar ServerState -> (String -> ReplT ReplState m ()) -> ReplT ReplState m ()
loop st o = do
    minput <- liftI $ getInputLine "Paradox> "
    case minput of
      Nothing -> o "Goodbye."
      Just input -> process st o input >> loop st o


newtype ReplT s m a = ReplT { unReplT :: InputT (StateT s m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadException)

instance MonadTrans (ReplT s) where
    lift = ReplT . lift . lift

instance Monad m => MonadState s (ReplT s m) where
    get = ReplT $ lift get
    put s = ReplT $ lift $ put s


runReplT :: ReplT s m a -> InputT (StateT s m) a
runReplT = unReplT

runReplM :: MonadException m => Behavior -> s -> ReplT s m a -> m (a, s)
runReplM b s m = runStateT (runInputTBehavior b defaultSettings (runReplT m)) s

liftI :: InputT (StateT s m) a -> ReplT s m a
liftI = ReplT



runOne :: (MonadIO m) => MVar ServerState -> (String -> m ()) -> TS.Text -> ReplState -> m ()
runOne st o expression ReplState {..} = do
    s <- liftIO $ computeTime rsStart
    e <- liftIO $ computeTime rsEnd
    innerProcess st o (textToCExpr expression) s e rsSamples rsRender


--runRepl :: MonadException m => MVar ServerState -> m ((), ReplState)
--runRepl :: MVar ServerState -> ReplM ()
runRepl :: MonadException m => MVar ServerState -> m ((), ReplState)
runRepl st = --evalStateT (runInputT defaultSettings (unReplM $ loop st)) (def :: ReplState)
    runReplM defaultBehavior (def :: ReplState) (loop st $ liftI . outputStrLn)
--runRepl :: MVar ServerState -> m ((), ReplState)
--runRepl st = (runInputT defaultSettings (runStateT (loop st) def))

runReplWithHandle :: MonadException m => Handle -> MVar ServerState -> m ((), ReplState)
runReplWithHandle h st = --evalStateT (runInputT defaultSettings (unReplM $ loop st)) (def :: ReplState)
    runReplM (useFileHandle h) (def :: ReplState) (loop st $ \s -> lift $ liftIO $  hPutStrLn h s)

data ReplState = ReplState {
        rsStart :: Time
      , rsEnd :: Time
      , rsRender :: String
      , rsSamples :: Integer
      } deriving (Show)

data Time = Waiting Integer (IO Integer)
          | SetTime Integer

instance Show Time where
    show (Waiting o _) = "Waiting " ++ show o
    show (SetTime t) = "SetTime " ++ show t

computeTime :: Time -> IO Integer
computeTime (SetTime t) = return t
computeTime (Waiting o f) = fmap (+ o) f

instance Default ReplState where
    def = ReplState { rsStart = Waiting (-60) $ floor <$> POSIX.getPOSIXTime
                    , rsEnd = Waiting 0 $ floor <$> POSIX.getPOSIXTime
                    , rsRender = "json"
                    , rsSamples = 600
                    }
