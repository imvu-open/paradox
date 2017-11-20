{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Paradox.Logic (
  readAndMkConfig
, mkServerState
, updateCounterMap
, updateCounterTree
, startKeysQuery
) where

import Prelude                                hiding (mapM)

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Paradox.Types
import Paradox.Logger
--import Paradox.Memo
import Paradox.Cache

import Data.Semigroup                         ( (<>)
                                              , Any
                                              )
import Data.Time.LocalTime.TimeZone.Olson     (getTimeZoneSeriesFromOlsonFile)
import Control.Monad                          ( when
                                              , unless
                                              , forever
                                              , void
                                              , join
                                              , forM_
                                              )
import System.FilePath                        ( takeDirectory
                                              , dropTrailingPathSeparator
                                              )
import System.Directory                       ( createDirectoryIfMissing
                                              , getPermissions
                                              , writable
                                              )

import Data.Traversable                       (forM)
import Control.Concurrent                     ( threadDelay
                                              , forkIO)
import Control.Exception.Lens                 (AsAsyncException (_AsyncException))
import Control.Lens                           ( Getting
                                              , has
                                              )
import Control.Exception                      (SomeException)
import Control.Monad.Catch                    ( MonadCatch(catch)
                                              , MonadMask
                                              , bracket
                                              , throwM
                                              )

import Paradox.Logger.Global                  (setGlobalLoggers)
import Paradox.Istatd.CounterMap              (CounterMap)
import Paradox.Istatd.Types                   (CountersMatch)

import qualified Control.Exception as E
import qualified Paradox.Render.LocalFonts    as LF
import qualified Paradox.Istatd.CounterTree   as CT
import qualified Data.ByteString.Lazy.Char8   as BSLC
import qualified Data.Text                    as TS
import qualified Data.Time.Clock.POSIX        as POSIX
import qualified Network.HTTP.Client          as Client
import qualified Network.HTTP.Client.Internal as ClientT
import qualified Data.Aeson                   as JSON
import qualified Network.HTTP.Client.TLS      as ClientTLS
import qualified Paradox.Istatd               as I
import qualified Paradox.Istatd.Types         as IT
import qualified Paradox.Istatd.Util          as ITU
import qualified Paradox.Istatd.AsyncClient   as IstatdClient


convertToConfig :: (String -> IO ())
                -> LoadedConfiguration
                -> IO Configuration
convertToConfig earlyLogger LoadedConfiguration {..} = do
    let cTarget                     = lcTarget
        cHost                       = lcHost
        cUser                       = lcUser
        cPassword                   = lcPassword
        cQueryTimer                 = lcQueryTimer
        cUseAuth                    = lcUseAuth
        cPort                       = lcPort
        cIstatdEnabled              = lcIstatdEnabled
        cIstatdHost                 = lcIstatdHost
        cIstatdPort                 = lcIstatdPort
        cIstatdCategories           = lcIstatdCategories
        cIstatdPrefix               = lcIstatdPrefix
        cServeBase                  = lcServeBase
        cSandboxedCommands          = lcSandboxedCommands
        cEventsConfig               = lcEventsConfig
        cLoggerConfig               = lcLoggerConfig
        cSMTPServer                 = lcSMTPServer
        cSourceAddr                 = lcSourceAddr
        cPermalinkDir               = lcPermalinkDir
        cPermalinkWaitTimeout       = lcPermalinkWaitTimeout
        cBaseUrl                    = lcBaseUrl
        cIstatdReqSize              = lcIstatdReqSize
        cReplPort                   = lcReplPort
        cTemplateDir                = lcTemplateDir


    cFontsSelector <- newEmptyMVar
    void $ forkIO $ do
        let fetchFonts = do
                mkFontSelector <- LF.localFonts lcFontsPath
                putMVar cFontsSelector mkFontSelector
        fetchFonts


    cLoggers <- case cLoggerConfig of
        Just config -> do
            loggers <- convertToLogger config
            return $ Just loggers
        _ -> return Nothing

    earlyLogger "Checking perms"
    forM_ [cPermalinkDir, cTemplateDir] $ \dir ->
      E.handle (\e -> earlyLogger $ show (e :: SomeException)) $ do
          perm <- getPermissions (takeDirectory $ dropTrailingPathSeparator dir)
          when (writable perm) $ do
              earlyLogger $ "making dir" ++ show dir
              createDirectoryIfMissing True dir

    return Configuration {..}


mkCacheState :: IO CacheState
mkCacheState = do
    csCounterMapCache <- newEmptyTMVarIO
    csCounterTreeCache <- newEmptyTMVarIO

    return CacheState {..}

mkServerState :: (String -> IO ())
              -> FilePath
              -> (LoadedConfiguration -> LoadedConfiguration)
              -> IO ServerState
mkServerState earlyLogger fp xform = do
    ssCacheState <- mkCacheState
    manager <- Client.newManager
      $ ClientTLS.tlsManagerSettings
      { Client.managerResponseTimeout = Client.responseTimeoutMicro 60000000 }
    Right ssReadConfigInit <- readAndMkConfig fp xform
    earlyLogger $ show ssReadConfigInit
    ssConfig <- convertToConfig earlyLogger ssReadConfigInit
    let ssManager = manager
        ssThread = Nothing

    logInfo ssConfig $ LogText "Setting up Istatd Reporting"
    ssIstatdLogger <- configureIstatd ssConfig
    logInfo ssConfig $ LogText "Setting up memoization"
    let
      ssMemo = \a k -> a k --fuzzyMemo ssIstatdLogger "cache" id
    logInfo ssConfig $ LogText "Setting up cacheing"
    ssCache <- fuzzyCache ssIstatdLogger "counter_cache"
    logInfo ssConfig $ LogText "Setting up timezones"
    ssTimeZoneSeries <- getTimeZoneSeriesFromOlsonFile "/etc/localtime"

    logInfo ssConfig $ LogText "Setting up global loggers"
    setGlobalLoggers $ cLoggers ssConfig

    logInfo ssConfig $ LogText $ "Read Config\n"
                                   <> TS.pack (show ssReadConfigInit)
    ssBarrier <- newEmptyMVar
    return ServerState {..}

readAndMkConfig :: FilePath
                -> (LoadedConfiguration -> LoadedConfiguration)
                -> IO (Either String LoadedConfiguration)
readAndMkConfig fp xform = do
    file <- BSLC.readFile fp
    return $ xform <$> mkFromConfig file

mkFromConfig :: BSLC.ByteString
             -> Either String LoadedConfiguration
mkFromConfig = JSON.eitherDecode


configureIstatd :: Configuration
                -> IO IstatdClient.Send
configureIstatd config =
    if cIstatdEnabled config
       then do
            let istatdHost = cIstatdHost config
                istatdPort = cIstatdPort config
                istatdCategories = cIstatdCategories config
                istatdPrefix = cIstatdPrefix config

            IstatdClient.start istatdHost
                            istatdPort
                            istatdPrefix
                            istatdCategories
       else return $ const (return ())

catchAllBut :: MonadMask m
            => Getting Any SomeException b
            -> m a
            -> (SomeException -> m a)
            -> m a
catchAllBut exceptionPrism action handler =
    catch action $ \(e :: SomeException) ->
      if has exceptionPrism e then throwM e else handler e

catchAllNoAsync :: MonadMask m
                => m a
                -> (SomeException -> m a)
                -> m a
catchAllNoAsync = catchAllBut _AsyncException

almostForever :: MonadMask m
              => m a
              -> (a -> m b)
              -> (a -> m c)
              -> (SomeException -> m c)
              -> m c
almostForever initial final action handler =
    forever (bracket initial final action `catchAllNoAsync` handler)

startKeysQuery :: MutableState
               -> IO ()
startKeysQuery state = mdo
    -- Use mdo (RecursiveDo) to allow letrec style passing in do so we can
    -- save the threadId
    threadId <- forkIO
              $ almostForever (return ())
                              (\() -> return ())
                              (\() -> do
                                  modifyMVar_ state $ \innerState -> return $ innerState { ssThread = Just threadId }
                                  loop state
                              )
                              (\e -> withMVar state $ \innerState -> logError (ssConfig innerState) . LogText . TS.pack . show $ e)
    return ()

delay :: Microseconds
      -> IO ()
delay us = threadDelay $ extractMicroseconds us

toMicroseconds :: POSIX.POSIXTime
               -> Microseconds
toMicroseconds t = Microseconds $ round (t * 1000000)


loop :: MutableState
     -> IO ()
loop state = do
    now <- POSIX.getPOSIXTime
    innerState <- readMVar state
    let conf = ssConfig innerState
    lastTs' <- fmap (join . fmap cmcTimeStamp) <$> atomically $ tryReadTMVar $ csCounterMapCache $ ssCacheState innerState
    let queryTimerUS = cQueryTimer conf
        nowUS = toMicroseconds now
        manager = ssManager innerState
        istatdLogger = ssIstatdLogger innerState

    logInfo conf $ LogText $ "Querying Counters: "
                          <> TS.pack (show now)

    let getAndCache = do
            istatdLogger $ IstatdClient.Counter $ "counter_list" <> "query" <> "started"

            res <- I.getCountersRaw manager conf "*"

            let respBody = ClientT.responseBody res

            forM (JSON.decode respBody) $ \ctrs -> do
                istatdLogger $ IstatdClient.Counter $ "counter_list" <> "query" <> "completed"

                ts <- POSIX.getPOSIXTime

                logInfo conf $ LogText $ "Replacing counter cache state: "
                                      <> TS.pack (show ts)

                let finalCtrs = ITU.countersReplyToCounterMap ctrs
                    mapping = IT.crTypeMapping ctrs
                    !counterTree = ITU.countersReplyToCounterTree ctrs
                    newTimeStamp = Just $ toMicroseconds ts
                replaceMVar (csCounterMapCache $ ssCacheState innerState) (CounterMapCache finalCtrs mapping newTimeStamp)
                replaceMVar (csCounterTreeCache $ ssCacheState innerState) (CounterTreeCache counterTree mapping newTimeStamp)

                istatdLogger $ IstatdClient.Counter $ "counter_list" <> "replaced"

                logInfo conf $ LogText $ "Delaying counter cache query for full time: "
                                      <> TS.pack (show queryTimerUS)

                istatdLogger $ IstatdClient.Gauge ("counter_list" <> "delay" <> "full")
                                                  (realToFrac $ extractMicroseconds queryTimerUS)
                delay queryTimerUS

    case lastTs' of
        Nothing -> void getAndCache
        Just lastTs ->
            if (nowUS - lastTs) >= queryTimerUS
               then void getAndCache
               else do
                    let delta = queryTimerUS - (nowUS - lastTs)

                    logInfo conf $ LogText $ "Delaying counter cache query for partial time: "
                                          <> TS.pack (show delta)

                    istatdLogger $ IstatdClient.Gauge ("counter_list" <> "delay" <> "partial")
                                                      (realToFrac $ extractMicroseconds delta)

                    delay delta

    loop state

replaceMVar :: TMVar a -> a -> IO ()
replaceMVar mvar val = void $ do
  wrote <- atomically $ tryPutTMVar mvar val
  unless wrote $ void $ atomically $ swapTMVar mvar val

timeAndState :: MutableState
             -> IO (POSIX.POSIXTime, ServerState)
timeAndState state = do
    ts <- POSIX.getPOSIXTime
    innerState <- readMVar state
    return (ts, innerState)

updateCounterMap :: MutableState
                 -> CounterMap CountersMatch
                 -> IT.CountersTypeMapping
                 -> IO ()
updateCounterMap state newMap mapping = do
    (ts, innerState) <- timeAndState state
    let conf = ssConfig innerState
    logInfo conf $ LogText $ "Updating Counter Map Cache"
                          <> TS.pack (show ts)
    let cacheState = ssCacheState innerState
        newTimeStamp = Just $ toMicroseconds ts
    void $ atomically $ swapTMVar (csCounterMapCache cacheState) (CounterMapCache newMap mapping newTimeStamp)

updateCounterTree :: MutableState
                  -> CT.SuperHashMap
                  -> IT.CountersTypeMapping
                  -> IO ()
updateCounterTree state !newTree mapping = do
    (ts, innerState) <- timeAndState state
    let conf = ssConfig innerState
    logInfo conf $ LogText $ "Updating Counter Map Tree"
                          <> TS.pack (show ts)
    let cacheState = ssCacheState innerState
        newTimeStamp = Just $ toMicroseconds ts
    void $ atomically $ swapTMVar (csCounterTreeCache cacheState) (CounterTreeCache newTree mapping newTimeStamp)
