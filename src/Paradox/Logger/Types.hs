{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module Paradox.Logger.Types (
  LogLevel(..)
, LogType(..)
, Loggers(..)
, Log(..)
, LogEntryValue(..)
, Logger(..)
, LoggerCmd(..)
) where

import Data.Map             (Map)
import Data.Aeson           ( FromJSON(..)
                            , withText
                            , ToJSON(..)
                            )

import Web.Scotty.Trans     (Parsable(..))

import qualified Data.Text  as TS

data LogLevel = Trace | Debug | Verbose | Info | Warning | Error | Critical | None deriving (Show, Eq, Read, Ord)

data Log = LogText TS.Text
         | LogData (Map TS.Text LogEntryValue)

data LogEntryValue = forall a. ToJSON a => MkJsonable a

instance ToJSON LogEntryValue where
    toJSON (MkJsonable a) = toJSON a

instance Parsable LogLevel where
    parseParam = \case
        "trace" -> Right Trace
        "debug" -> Right Debug
        "verbose" -> Right Verbose
        "info" -> Right Info
        "warning" -> Right Warning
        "error" -> Right Error
        "critical" -> Right Critical
        _ -> Left "Unknown log level"


instance FromJSON LogLevel where
    parseJSON = withText "FromJSON LogLevel" $ \case
        "trace" -> return Trace
        "debug" -> return Debug
        "info" -> return Info
        "verbose" -> return Verbose
        "warning" -> return Warning
        "error" -> return Error
        "critical" -> return Critical
        _ -> fail "Failed to parse LogLevel"

instance ToJSON LogLevel where
    toJSON = \case
        Trace       ->   "trace"
        Debug       ->   "debug"
        Info        ->   "info"
        Verbose     ->   "verbose"
        Warning     ->   "warning"
        Error       ->   "error"
        Critical    ->   "critical"
        None        ->   "none"

data LogType = Access | File | StdOut | StdErr deriving (Show, Eq, Read)

instance FromJSON LogType where
    parseJSON = withText "FromJSON LogType String" $ \case
        "stderr" -> return StdErr
        "stdout" -> return StdOut
        "file" -> return File
        "access" -> return Access
        _ -> fail "Failed to parse logtype"

instance ToJSON LogType where
    toJSON = \case
        StdErr      ->   "stderr"
        StdOut      ->   "stdout"
        File        ->   "file"
        Access      ->   "access"

data Loggers = Loggers {
          logLevel :: LogLevel -> Log -> IO ()
        , logSpecific :: TS.Text -> Log -> IO ()
        , cmdToLog :: TS.Text -> LoggerCmd -> IO ()
    }

data Logger = Logger {
      loggerFn :: LogLevel -> Log -> IO ()
    , loggerCmd :: LoggerCmd -> IO ()
    }

data LoggerCmd = LogClose | LogRenew

instance Show Loggers where
    show Loggers{} = "Logging Functions"

