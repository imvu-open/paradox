module Paradox.Logger.Global (
  module Paradox.Logger.Types
, setGlobalLoggers
, logTraceG
, logDebugG
, logVerboseG
, logInfoG
, logWarningG
, logErrorG
, logCriticalG
, logToSpecificG
, logTraceGM
, logDebugGM
, logVerboseGM
, logInfoGM
, logWarningGM
, logErrorGM
, logCriticalGM
, logToSpecificGM
) where

import Control.Concurrent.MVar
import Paradox.Logger.Types
import Control.Monad.IO.Class

import System.IO.Unsafe         (unsafePerformIO)
import Control.Monad            (void)

import qualified Data.Text      as TS

{-# NOINLINE logG #-}
logG :: MVar (Maybe Loggers)
logG = unsafePerformIO $ newMVar Nothing

setGlobalLoggers :: Maybe Loggers -> IO ()
setGlobalLoggers v = void $ swapMVar logG v

logLevelG' :: LogLevel -> Log -> IO ()
logLevelG' level bs = do
    loggers' <- readMVar logG
    case loggers' of
        Just loggers -> logLevel loggers level bs
        Nothing -> return ()

logTraceG :: Log -> IO ()
logTraceG = logLevelG' Trace

logDebugG :: Log -> IO ()
logDebugG = logLevelG' Debug

logInfoG :: Log -> IO ()
logInfoG = logLevelG' Info

logVerboseG :: Log -> IO ()
logVerboseG = logLevelG' Verbose

logWarningG :: Log -> IO ()
logWarningG = logLevelG' Warning

logErrorG :: Log -> IO ()
logErrorG = logLevelG' Error

logCriticalG :: Log -> IO ()
logCriticalG = logLevelG' Critical

logToSpecificG :: TS.Text -> Log -> IO ()
logToSpecificG loggerName bs = do
    loggers' <- readMVar logG
    case loggers' of
        Just loggers -> logSpecific loggers loggerName bs
        Nothing -> return ()

logTraceGM :: MonadIO m => Log -> m ()
logTraceGM = liftIO . logLevelG' Trace

logDebugGM :: MonadIO m => Log -> m ()
logDebugGM = liftIO . logLevelG' Debug

logInfoGM :: MonadIO m => Log -> m ()
logInfoGM = liftIO . logLevelG' Info

logVerboseGM :: MonadIO m => Log -> m ()
logVerboseGM = liftIO . logLevelG' Verbose

logWarningGM :: MonadIO m => Log -> m ()
logWarningGM = liftIO . logLevelG' Warning

logErrorGM :: MonadIO m => Log -> m ()
logErrorGM = liftIO . logLevelG' Error

logCriticalGM :: MonadIO m => Log -> m ()
logCriticalGM = liftIO . logLevelG' Critical

logToSpecificGM :: MonadIO m => TS.Text -> Log -> m ()
logToSpecificGM loggerName bs = do
    loggers' <- liftIO $ readMVar logG
    case loggers' of
        Just loggers -> liftIO $ logSpecific loggers loggerName bs
        Nothing -> return ()
