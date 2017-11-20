{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Paradox.Types (
  RScottyM
, RActionM
, MutableState
, CacheState (..)
, CounterMapCache (..)
, CounterTreeCache (..)
, ServerState (..)
, Configuration (..)
, LoadedConfiguration (..)
, Microseconds (..)
, CommandConfig (..)
, EventConfig (..)
, SensitiveData (..)
, LogConfig (..)
, InnerMonadM (..)
, Except (..)
, extractMicroseconds
, doWithState
, doWithState'
, innerMonadM
, getFromState
, eventConfigToUrl
, fromCVInt
, fromCVString
, getHost
, getTarget
, getUser
, getPassword
, getQueryTimer
, getUseAuth
, getServeBase
, getSandboxedCommands
, getEventConfig
) where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TMVar
import Paradox.Memo
import Paradox.Cache
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Default

import Paradox.Shell.Types
import Paradox.Logger.Types
import Control.Monad                            ( join
                                                , when
                                                )
import Control.Monad.IO.Class                   (MonadIO)
import Control.Monad.Reader.Class               ( MonadReader
                                                , ask
                                                , asks
                                                )
import Control.Monad.Reader                     (ReaderT(..))
import Control.Monad.Trans.Class                ( MonadTrans
                                                , lift
                                                )
import Data.Maybe                               ( fromMaybe
                                                , isNothing
                                                )
import Control.Concurrent                       (ThreadId)
import Data.Aeson                               ( FromJSON(..)
                                                , (.:)
                                                , withObject
                                                , Value(..)
                                                , (.:?)
                                                , (.!=)
                                                , (.=)
                                                , object
                                                , ToJSON(..)
                                                )

import Data.Time.LocalTime.TimeZone.Series      (TimeZoneSeries)


import Paradox.Events.Types                     (EventFilter (..))

import Paradox.Istatd.CounterMap                (CounterMap)
import Paradox.Istatd.CounterTree               (SuperHashMap)
import Paradox.Istatd.Types                     ( CountersMatch
                                                , CountersTypeMapping
                                                , PostReplyRet
                                                , PostReply
                                                )
import Paradox.Istatd.Cache.Types               (CountersReq)
import Imvu.Network.IstatdClient                ( Name
                                                , nameFromBytes
                                                )

import Graphics.Rendering.Chart.Backend.Diagrams (FontSelector)
import Paradox.Error.Types                      (Except(..))

import qualified Data.Text                      as TS
import qualified Data.Map.Strict                as M
import qualified Data.Text.Lazy.Encoding        as TLE
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.ByteString.Lazy.Char8     as BSLC
import qualified Network.HTTP.Client            as Client
import qualified Web.Scotty.Trans               as WST
import qualified Paradox.Istatd.AsyncClient     as IstatdClient
import qualified Network                        as Net


type RScottyM = WST.ScottyT Except InnerMonadM
type RActionM = WST.ActionT Except InnerMonadM

newtype InnerMonadM a = InnerMonadM { runInnerMonadM :: ReaderT MutableState IO a }
    deriving( Applicative, Functor, Monad, MonadIO, MonadReader MutableState, MonadBase IO)

instance MonadBaseControl IO InnerMonadM where
    type StM InnerMonadM a = a
    liftBaseWith f = InnerMonadM $ liftBaseWith $ \q -> f (q . runInnerMonadM)
    restoreM = InnerMonadM . restoreM

innerMonadM :: MonadTrans t
            => InnerMonadM a
            -> t InnerMonadM a
innerMonadM = lift

doWithState :: MonadTrans t
            => forall a.
               (MutableState -> InnerMonadM a)
            -> t InnerMonadM a
doWithState = doWithState' id

doWithState' :: MonadTrans t
             => forall a c.
                (MutableState -> c)
             -> (c -> InnerMonadM a)
             -> t InnerMonadM a
doWithState' fAsk fWithState = innerMonadM $ asks fAsk >>= fWithState

getFromState :: MonadTrans t
             => forall c.
                (MutableState -> InnerMonadM c)
             -> t InnerMonadM c
getFromState extract = innerMonadM $ ask >>= extract

newtype Microseconds = Microseconds Int deriving Show

extractMicroseconds :: Microseconds
                    -> Int
extractMicroseconds (Microseconds us) = us

instance Num Microseconds where
    Microseconds s + Microseconds s' = Microseconds (s + s')

    Microseconds s - Microseconds s' = Microseconds (s - s')
    Microseconds s * Microseconds s' = Microseconds (s * s')
    abs (Microseconds s) = Microseconds $ abs s
    signum (Microseconds s) = Microseconds $ signum s
    fromInteger s = Microseconds $ fromIntegral s

instance Eq Microseconds where
    Microseconds s == Microseconds s' = s == s'

instance Ord Microseconds where
    Microseconds s `compare` Microseconds s' = s `compare` s'

data CounterTreeCache = CounterTreeCache {
      ctcCounterTree :: !SuperHashMap
    , ctcTypeMapping :: !CountersTypeMapping
    , ctcTimeStamp   :: !(Maybe Microseconds)
    }

data CounterMapCache = CounterMapCache {
      cmcCounterMap  :: !(CounterMap CountersMatch)
    , cmcTypeMapping :: !CountersTypeMapping
    , cmcTimeStamp   :: !(Maybe Microseconds)
    }

data CacheState = CacheState {
      csCounterMapCache  :: !(TMVar CounterMapCache)
    , csCounterTreeCache :: !(TMVar CounterTreeCache)
    }

data CommandConfig = CommandConfig {
      ccCommand :: !ShellCommand
    , ccDelim   :: !TS.Text
    , ccArgs    :: ![ShellArgument]
    } deriving (Show)

instance FromJSON CommandConfig where
    parseJSON = withObject "FromJSON CommandConfig" $ \o ->
        CommandConfig <$> o .: "command"
                      <*> o .: "delim"
                      <*> o .: "args"

data EventConfig = EventConfig {
      ecProto    :: !BSL.ByteString
    , ecAddr     :: !BSL.ByteString
    , ecPort     :: !Int
    , ecEndpoint :: !BSL.ByteString
    , ecFilter   :: !EventFilter
    } deriving (Show)

instance FromJSON EventConfig where
    parseJSON = withObject "FromJSON EventConfig" $ \o ->
        let mkByteString = fmap TLE.encodeUtf8
        in EventConfig
            <$> mkByteString (o .: "proto")
            <*> mkByteString (o .: "addr")
            <*> o .: "port"
            <*> mkByteString (o .: "endpoint")
            <*> (parseJSON =<< (o .: "filter"))

eventConfigToUrl :: EventConfig
                 -> String
eventConfigToUrl EventConfig{..} = BSLC.unpack $
    BSLC.concat [ecProto, "://", ecAddr, ":", BSLC.pack $ show ecPort, ecEndpoint]

newtype SensitiveData a = SensitiveData { unSensitiveData :: a }

instance Show (SensitiveData a) where
    show = const "SensitiveData"



data ConfigValue = CVString String
                 | CVInt Int
                 deriving (Show, Eq, Read)

fromCVInt :: ConfigValue
          -> Int
fromCVInt (CVInt i) = i
fromCVInt _ = error "Bad Config value"

fromCVString :: ConfigValue
             -> String
fromCVString (CVString s) = s
fromCVString _ = error "Bad Config value"

instance FromJSON ConfigValue where
    parseJSON = \case
        n@Number {} -> CVInt <$> parseJSON n
        t@String {} -> CVString <$> parseJSON t
        _ -> fail "Log config only supports number or string"

instance ToJSON ConfigValue where
    toJSON = \case
        CVInt i -> toJSON i
        CVString s -> toJSON s

data LogConfig = LogConfig {
      lConfig :: M.Map TS.Text ConfigValue
    , lType   :: LogType
    , lLevel  :: LogLevel
    } deriving (Show)

instance Default LogConfig where
    def = LogConfig { lLevel = Trace
                    , lType = StdOut
                    , lConfig = mempty }

instance FromJSON LogConfig where
    parseJSON = withObject "FromJSON LogConfig" $ \o -> do
        lLevel <- o .: "level"
        lType <- o .: "type"
        lConfig <- o .: "config"
        when (lType == File) $
            when (isNothing $ M.lookup "path" lConfig) (fail "need a path")
        return LogConfig {..}

instance ToJSON LogConfig where
    toJSON LogConfig {..} = object [
          "level" .= lLevel
        , "type" .= lType
        , "config" .= lConfig
        ]

data LoadedConfiguration = LoadedConfiguration {
      lcTarget                  :: !BSL.ByteString
    , lcHost                    :: !BSL.ByteString
    , lcUser                    :: !BSL.ByteString
    , lcPassword                :: !(SensitiveData BSL.ByteString)
    , lcQueryTimer              :: !Microseconds
    , lcUseAuth                 :: !Bool
    , lcPort                    :: !Int
    , lcIstatdEnabled           :: !Bool
    , lcIstatdHost              :: !Net.HostName
    , lcIstatdPort              :: !Net.PortNumber
    , lcIstatdCategories        :: !FilePath
    , lcIstatdPrefix            :: !Name
    , lcServeBase               :: !BSL.ByteString
    , lcSandboxedCommands       :: !(Maybe (M.Map TS.Text CommandConfig))
    , lcLoggerConfig            :: !(Maybe (M.Map TS.Text LogConfig))
    , lcEventsConfig            :: !(Maybe (M.Map TS.Text EventConfig))
    , lcSMTPServer              :: !(Maybe String)
    , lcSourceAddr              :: !(Maybe TS.Text)
    , lcFontsPath               :: !String
    , lcPermalinkDir            :: !String
    , lcPermalinkWaitTimeout    :: !Integer
    , lcBaseUrl                 :: !String
    , lcIstatdReqSize           :: !(Maybe Int)
    , lcReplPort                :: !Net.PortID
    , lcTemplateDir             :: !String
    } deriving (Show)

data Configuration = Configuration {
      cTarget                   :: !BSL.ByteString
    , cHost                     :: !BSL.ByteString
    , cUser                     :: !BSL.ByteString
    , cPassword                 :: !(SensitiveData BSL.ByteString)
    , cQueryTimer               :: !Microseconds
    , cUseAuth                  :: !Bool
    , cPort                     :: !Int
    , cIstatdEnabled            :: !Bool
    , cIstatdHost               :: !Net.HostName
    , cIstatdPort               :: !Net.PortNumber
    , cIstatdCategories         :: !FilePath
    , cIstatdPrefix             :: !Name
    , cServeBase                :: !BSL.ByteString
    , cSandboxedCommands        :: !(Maybe (M.Map TS.Text CommandConfig))
    , cLoggers                  :: !(Maybe Loggers)
    , cLoggerConfig             :: !(Maybe (M.Map TS.Text LogConfig))
    , cEventsConfig             :: !(Maybe (M.Map TS.Text EventConfig))
    , cSMTPServer               :: !(Maybe String)
    , cSourceAddr               :: !(Maybe TS.Text)
    , cFontsSelector            :: !(MVar (FontSelector Double))
    , cPermalinkDir             :: !String
    , cPermalinkWaitTimeout     :: !Integer
    , cBaseUrl                  :: !String
    , cIstatdReqSize            :: !(Maybe Int)
    , cReplPort                 :: !Net.PortID
    , cTemplateDir              :: !String
    }

instance FromJSON LoadedConfiguration where
    parseJSON = withObject "FromJSON LoadedConfiguration" $ \o -> do
        let mkByteString = fmap TLE.encodeUtf8
        lcTarget <- mkByteString $ o .: "target"
        lcHost <- mkByteString $ o .:? "host" .!= ""
        lcUseAuth <- o .:? "use_auth" .!= False
        lcPassword <- if lcUseAuth
            then fmap (SensitiveData . TLE.encodeUtf8) $ o .: "password"
            else return $ SensitiveData ""
        lcUser <- if lcUseAuth
            then mkByteString $ o .: "user"
            else return ""
        lcQueryTimer <- fmap Microseconds $ o .: "query_timer"
        lcPort <- fmap fromInteger $ o .:? "port" .!= 1313
        lcIstatdEnabled <- o .:? "istatd_agent_enabled" .!= False
        lcIstatdHost <- o .:? "istatd_agent_host" .!= "localhost"
        lcIstatdPort <- fmap fromInteger $ o .:? "istatd_agent_port" .!= 8111
        lcIstatdCategories <- o .:? "istatd_agent_categories_file" .!= "/etc/istatd.categories"
        tempPrefix <- o .:? "istatd_agent_prefix"
        let lcIstatdPrefix = fromMaybe "paradox"
                                      ( join
                                      $ (nameFromBytes . BSLC.toStrict . TLE.encodeUtf8)
                                      <$> tempPrefix
                                      )
        lcServeBase <- mkByteString $ o .:? "serve_base" .!= ""
        lcSandboxedCommands <- o .:? "sandboxed_commands"
        lcLoggerConfig <- o .:? "logger_config"
        lcEventsConfig <- o .:? "events_config"
        lcSMTPServer <- o .:? "smtp_server"
        lcSourceAddr <- o .:? "source_addr"
        lcFontsPath <- o .: "fonts_path"
        lcPermalinkDir <- o .: "permalink_dir"
        lcPermalinkWaitTimeout <- o .:? "permalink_wait_timeout" .!= 15000000
        lcBaseUrl <- o .: "base_url"
        lcIstatdReqSize <- o .:? "istatd_req_size"
        lcReplPort <- fmap (Net.PortNumber . fromInteger) $ o .: "repl_port"
        lcTemplateDir <- o .: "template_dir"

        return LoadedConfiguration {..}

data ServerState = ServerState {
      ssCacheState     :: !CacheState
    , ssConfig         :: !Configuration
    , ssManager        :: !Client.Manager
    , ssThread         :: !(Maybe ThreadId)
    , ssBarrier        :: !(MVar ())
    , ssIstatdLogger   :: !IstatdClient.Send
    , ssMemo           :: !(MemoFn IO CountersReq PostReplyRet)
    , ssCache          :: !(CacheFn CountersReq PostReply)
    , ssTimeZoneSeries :: !TimeZoneSeries
    }

type MutableState = MVar ServerState


getHost :: Configuration
        -> BSL.ByteString
getHost = cHost

getTarget :: Configuration
          -> BSL.ByteString
getTarget = cTarget

getUser :: Configuration
        -> BSL.ByteString
getUser = cUser

getPassword :: Configuration
            -> BSL.ByteString
getPassword = unSensitiveData . cPassword

getQueryTimer :: Configuration
              -> Microseconds
getQueryTimer = cQueryTimer

getUseAuth :: Configuration
           -> Bool
getUseAuth = cUseAuth

getServeBase :: Configuration
             -> BSL.ByteString
getServeBase = cServeBase

getSandboxedCommands  :: Configuration
                      -> Maybe (M.Map TS.Text CommandConfig)
getSandboxedCommands = cSandboxedCommands

getEventConfig :: Configuration
               -> Maybe (M.Map TS.Text EventConfig)
getEventConfig = cEventsConfig
