{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Paradox.Handlers.Log where

import Control.Concurrent.MVar
import Control.Monad.IO.Class

import Paradox.Logger
import Data.Maybe                               (fromJust)

import Data.Monoid                              ((<>))
import Paradox.Logger.Global                    (setGlobalLoggers)
import Paradox.Types                            (RActionM)
import Paradox.State                            ( getMutableState'
                                                , withState
                                                )
import Network.HTTP.Types                       ( status200
                                                , status400
                                                )

import qualified Web.Scotty.Trans               as WST
import qualified Paradox.Types                  as PT
import qualified Data.Map.Strict                as M
import qualified Data.Aeson                     as JSON
import qualified Data.Text                      as TS

replaceLogConfig :: MonadIO m
                 => PT.ServerState
                 -> TS.Text
                 -> PT.LogConfig
                 -> m (PT.ServerState, M.Map TS.Text PT.LogConfig)
replaceLogConfig innerState name config = do
    let appConfig = PT.ssConfig innerState
        cfg = fromJust $ PT.cLoggerConfig appConfig
        loggers = fromJust $ PT.cLoggers appConfig
        cfg' = M.insert name config cfg
        appConfig' = appConfig { PT.cLoggerConfig = Just cfg' }
    liftIO $ closeLogs cfg loggers
    loggers' <- liftIO $ convertToLogger cfg'
    let finalConfig = appConfig' { PT.cLoggers = Just loggers' }
        innerState' = innerState { PT.ssConfig = finalConfig }
    liftIO $ setGlobalLoggers $ PT.cLoggers finalConfig
    return (innerState', cfg')

renewLogs :: MonadIO m
          => PT.ServerState
          -> m (PT.ServerState, M.Map TS.Text PT.LogConfig)
renewLogs innerState = do
    let appConfig = PT.ssConfig innerState
        cfg = fromJust $ PT.cLoggerConfig appConfig
        loggers = fromJust $ PT.cLoggers appConfig
    liftIO $ logInfo appConfig $ LogText $ "Renewing loggers using config: (" <> TS.pack (show cfg) <> ")"
    liftIO $ closeLogs cfg loggers
    loggers' <- liftIO $ convertToLogger cfg
    let appConfig' = appConfig { PT.cLoggers = Just loggers' }
        innerState' = innerState { PT.ssConfig = appConfig' }
    liftIO $ setGlobalLoggers $ PT.cLoggers appConfig'
    return (innerState', cfg)


getLogConfig :: RActionM ()
getLogConfig = do
    logConfig <- getMutableState' (PT.cLoggerConfig . PT.ssConfig)
    WST.json logConfig

getLogConfigByName :: RActionM ()
getLogConfigByName = do
    name <- WST.param "name"
    logConfig <- getMutableState' (fmap (M.lookup name) . PT.cLoggerConfig . PT.ssConfig)
    WST.json logConfig

postLogConfigByName :: RActionM ()
postLogConfigByName = do
    name <- WST.param "name"
    b' <- WST.body
    let v' = JSON.eitherDecode b' :: Either String PT.LogConfig
    case v' of
        Right config -> do
            cfg'' <- withState $ \s ->
                liftIO $ modifyMVar s $ \innerState ->
                    replaceLogConfig innerState name config
            WST.status status200
            WST.json cfg''
        Left err -> do
            WST.status status400
            WST.json $ M.fromList [(TS.pack "error", TS.pack ("malformed log config json " ++ err))]

postLogConfigByName' :: RActionM ()
postLogConfigByName' = do
    name <- WST.param "name"
    (level :: LogLevel) <- WST.param "level"
    cfg'' <- withState $ \s -> do
        modifiedCfg <- liftIO $ withMVar s $ \innerState -> do
            let appConfig = PT.ssConfig innerState
                cfg = fromJust $ PT.cLoggerConfig appConfig

                myCfg = fromJust $ M.lookup name cfg
                myCfg' = myCfg { PT.lLevel = level }
            return myCfg'
        liftIO $ modifyMVar s $ \innerState ->
            replaceLogConfig innerState name modifiedCfg
    WST.status status200
    WST.json cfg''

postRenewLogs :: RActionM ()
postRenewLogs = do
    r <- withState $ \s ->
        liftIO $ modifyMVar s $ \innerState ->
            renewLogs innerState

    WST.status status200
    WST.json r
