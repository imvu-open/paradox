{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Paradox.Logger (
  module Paradox.Logger.Types
, convertToLogger
, closeLogs
, logTrace
, logDebug
, logVerbose
, logInfo
, logWarning
, logError
, logCritical
, logToSpecific
, logTraceM
, logDebugM
, logVerboseM
, logInfoM
, logWarningM
, logErrorM
, logCriticalM
, logToSpecificM
) where

import Prelude                                  hiding ( mapM
                                                       , log
                                                       )

import Paradox.Types
import Paradox.Logger.Types

import Control.Monad.IO.Class

import Data.Traversable                         (mapM)
import Data.Maybe                               ( fromMaybe
                                                , fromJust
                                                )
import Control.Monad                            (when)
import Data.Foldable                            (forM_)
import Data.Monoid                              ((<>))
import Data.Time.Clock.POSIX                    (getPOSIXTime)

import qualified Data.Aeson                     as J
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.ByteString.Lazy.Char8     as BSLC
import qualified Data.Text                      as TS
import qualified Data.Map.Strict                as M
import qualified System.Log.FastLogger          as FLog

logLevel' :: Configuration -> LogLevel -> Log -> IO ()
logLevel' conf level log = case cLoggers conf of
    Just loggers -> logLevel loggers level log
    Nothing -> return ()

logTrace :: Configuration -> Log -> IO ()
logTrace conf = logLevel' conf Trace

logDebug :: Configuration -> Log -> IO ()
logDebug conf = logLevel' conf Debug

logInfo :: Configuration -> Log -> IO ()
logInfo conf = logLevel' conf Info

logVerbose :: Configuration -> Log -> IO ()
logVerbose conf = logLevel' conf Verbose

logWarning :: Configuration -> Log -> IO ()
logWarning conf = logLevel' conf Warning

logError :: Configuration -> Log -> IO ()
logError conf = logLevel' conf Error

logCritical :: Configuration -> Log -> IO ()
logCritical conf = logLevel' conf Critical

logToSpecific :: Configuration -> TS.Text -> Log -> IO ()
logToSpecific conf loggerName log = case cLoggers conf of
    Just loggers -> logSpecific loggers loggerName log
    Nothing -> return ()


logTraceM :: MonadIO m => Configuration -> Log -> m ()
logTraceM conf = liftIO . logLevel' conf Trace

logDebugM :: MonadIO m => Configuration -> Log -> m ()
logDebugM conf = liftIO . logLevel' conf Debug

logInfoM :: MonadIO m => Configuration -> Log -> m ()
logInfoM conf = liftIO . logLevel' conf Info

logVerboseM :: MonadIO m => Configuration -> Log -> m ()
logVerboseM conf = liftIO . logLevel' conf Verbose

logWarningM :: MonadIO m => Configuration -> Log -> m ()
logWarningM conf = liftIO . logLevel' conf Warning

logErrorM :: MonadIO m => Configuration -> Log -> m ()
logErrorM conf = liftIO . logLevel' conf Error

logCriticalM :: MonadIO m => Configuration -> Log -> m ()
logCriticalM conf = liftIO . logLevel' conf Critical

logToSpecificM :: MonadIO m => Configuration -> TS.Text -> Log -> m ()
logToSpecificM conf loggerName log = case cLoggers conf of
    Just loggers -> liftIO $ logSpecific loggers loggerName log
    Nothing -> return ()

closeLogs :: M.Map TS.Text LogConfig -> Loggers -> IO ()
closeLogs logConfigs Loggers{..} = do
    let keys = M.keys logConfigs
    forM_ keys $ \k -> cmdToLog k LogClose

convertToLogger :: M.Map TS.Text LogConfig -> IO Loggers
convertToLogger logConfigs = do
    loggers <- mapM mkLogger logConfigs
    let logLevel level log = forM_ loggers $ \Logger {..} -> loggerFn level log
        logSpecific name log = case M.lookup name loggers of
            Nothing -> return ()
            Just Logger {..} -> loggerFn None log
        cmdToLog name cmd = case M.lookup name loggers of
            Nothing -> return ()
            Just Logger {..} -> loggerCmd cmd
    return Loggers {..}

mkLogger :: LogConfig -> IO Logger
mkLogger LogConfig{..} =
    let guardLevel inL gL = when (inL >= gL)
    in case lType of
        File -> do
            let bufSize = fromMaybe FLog.defaultBufSize $ fromCVInt <$> M.lookup "bufferSize" lConfig
                path = fromJust $ fromCVString <$> M.lookup "path" lConfig
            logger <- FLog.newFileLoggerSet bufSize path
            let loggerFn level log = guardLevel level lLevel $ logElement logger level log
                loggerCmd = \case
                    LogClose -> FLog.rmLoggerSet logger
                    LogRenew -> FLog.renewLoggerSet logger
            return Logger {..}
        StdOut -> do
            let bufSize = fromMaybe FLog.defaultBufSize $ fromCVInt <$> M.lookup "bufferSize" lConfig
            logger <- FLog.newStdoutLoggerSet bufSize
            let loggerFn level log = guardLevel level lLevel $ logElement logger level log
                loggerCmd = \case
                    LogClose -> FLog.rmLoggerSet logger
                    LogRenew -> FLog.renewLoggerSet logger
            return Logger {..}
        StdErr -> do
            let bufSize = fromMaybe FLog.defaultBufSize $ fromCVInt <$> M.lookup "bufferSize" lConfig
            logger <- FLog.newStderrLoggerSet bufSize
            let loggerFn level log = guardLevel level lLevel $ logElement logger level log
                loggerCmd = \case
                    LogClose -> FLog.rmLoggerSet logger
                    LogRenew -> FLog.renewLoggerSet logger
            return Logger {..}
        Access -> do
            let bufSize = fromMaybe FLog.defaultBufSize $ fromCVInt <$> M.lookup "bufferSize" lConfig
                path = fromCVString <$> M.lookup "path" lConfig
            logger <- case path of
                Nothing -> FLog.newStdoutLoggerSet bufSize
                Just path' -> FLog.newFileLoggerSet bufSize path'
            let loggerFn level log = when (level == None) $ logElement logger level log
                loggerCmd = \case
                    LogClose -> FLog.rmLoggerSet logger
                    LogRenew -> FLog.renewLoggerSet logger
            return Logger {..}

addLevel :: LogLevel -> M.Map TS.Text LogEntryValue -> M.Map TS.Text LogEntryValue
addLevel l m = case l of
    None -> m
    level -> M.insert "level" (MkJsonable $ TS.pack $ show level) m

logElement :: FLog.LoggerSet -> LogLevel -> Log -> IO ()
logElement logger level log = do
    ts <- getPOSIXTime
    logElementWithTime logger level (floor ts) log

logElementWithTime :: FLog.LoggerSet -> LogLevel -> Integer -> Log -> IO ()
logElementWithTime logger level ts (LogText text) =
    let logMap :: M.Map TS.Text LogEntryValue
        logMap = addLevel level $ M.fromList [ ("message", MkJsonable text)
                                             , ("timestamp", MkJsonable ts)
                                             ]
    in doLog logger (J.encode logMap)
logElementWithTime logger level ts (LogData m) =
    let bs = J.encode $ addLevel level $ M.insert "timestamp" (MkJsonable ts) m
    in doLog logger bs

doLog :: FLog.LoggerSet -> BSL.ByteString -> IO ()
doLog logger bs =
    let nl = (FLog.toLogStr $ BSLC.pack "\n")
    in FLog.pushLogStr logger (FLog.toLogStr bs <> nl)
