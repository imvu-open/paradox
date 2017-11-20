{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Paradox.Main (
  main
) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Data.Monoid                          ((<>))
import Data.Default

import Paradox.Error                        ( uncaughtExceptionHandler )
import Paradox.Logger                       ( logInfo
                                            , Log(..)
                                            )
import GHC.Conc.Sync                        (setUncaughtExceptionHandler)
import System.Exit                          (exitSuccess)


import Network.Wai.Middleware.RequestLogger ( mkRequestLogger
                                            , RequestLoggerSettings(..)
                                            , Destination(..)
                                            , OutputFormat(..)
                                            , IPAddrSource(..)
                                            )
import Data.Maybe                           ( isJust
                                            , fromJust
                                            )
import System.Log.FastLogger                (fromLogStr)
import Options.Applicative                  ( Parser
                                            , argument
                                            , execParser
                                            , info
                                            , helper
                                            , fullDesc
                                            , help
                                            , switch
                                            , metavar
                                            , str
                                            , long
                                            , short
                                            , value
                                            , command
                                            , optional
                                            , subparser
                                            , progDesc
                                            , strOption
                                            , option
                                            , auto
                                            , eitherReader
                                            , ReadM
                                            )

import Paradox.Version                      (versionString)

import Paradox.Repl                         ( runRepl
                                            , runOne
                                            , runReplWithHandle
                                            , ReplState (..)
                                            , Time (..)
                                            )
import Control.Applicative                  ((<|>))
import Data.List                            (stripPrefix)

import qualified Paradox.Logger.Global      as LG
import qualified Data.Text                  as TS
import qualified Data.Text.Encoding         as TSE
import qualified Paradox.Routes             as Routes
import qualified Paradox.Logic              as PL
import qualified Paradox.Types              as PT
import qualified Web.Scotty.Trans           as WST
import qualified Paradox.Handlers.Log       as HL
import qualified System.Posix.Signals       as PSig
import qualified Data.Map.Strict            as M
import qualified Data.Time.Clock.POSIX      as POSIX
import qualified System.IO                  as SIO
import qualified Network                    as Net
import Control.Concurrent (forkIO)



data CliArguments = CliArguments
    { evalOne :: !(Maybe EvalOne)
    , testOnly :: !Bool
    , evalRepl :: !Bool
    , configFile :: !FilePath
    , version :: !Bool
    } deriving (Show)

runActionIntoIO :: MVar PT.ServerState -> PT.InnerMonadM a -> IO a
runActionIntoIO s m = runReaderT (PT.runInnerMonadM m) s

runOpts :: IO PT.ServerState
runOpts = do
    CliArguments {..} <- parseArgs
    when testOnly $ do
        conf <- PL.readAndMkConfig configFile id
        print conf
        exitSuccess
    when version $ do
        putStrLn versionString
        exitSuccess
    when (evalRepl || isJust evalOne) $ do
      realState <- PL.mkServerState (const $ return ()) configFile removeStdOutLoggers
      state <- newMVar realState
      PL.startKeysQuery state
      when evalRepl $ void $ runRepl state
      forM_ evalOne $ \evalOne' -> do
        let EvalOne {..} = evalOne'
            replState@ReplState {..} = def
            replState' = replState { rsStart = fromJust $ eoStart <|> Just rsStart
                                   , rsEnd = fromJust $ eoEnd <|> Just rsEnd
                                   , rsSamples = fromJust $ eoSamples <|> Just rsSamples
                                   , rsRender = fromJust $ eoRender <|> Just rsRender
                                   }
        runOne state putStrLn (TS.pack eoExpr) replState'

      exitSuccess
    PL.mkServerState putStrLn configFile id

removeStdOutLoggers :: PT.LoadedConfiguration -> PT.LoadedConfiguration
removeStdOutLoggers lc@PT.LoadedConfiguration { PT.lcLoggerConfig } = lc { PT.lcLoggerConfig = removeStdOutLoggers' lcLoggerConfig }
  where
    removeStdOutLoggers' (Just lcLoggers) = Just $ removeStdOutLoggers'' lcLoggers
    removeStdOutLoggers' Nothing = Nothing

    removeStdOutLoggers'' = M.filter analyze

    analyze PT.LogConfig {..} = lType /= LG.StdOut

parseArgs :: IO CliArguments
parseArgs = execParser $ info (helper <*> parseCliArgs) fullDesc

data EvalOne = EvalOne {
               eoExpr :: String
             , eoStart :: Maybe Time
             , eoEnd :: Maybe Time
             , eoSamples :: Maybe Integer
             , eoRender :: Maybe String
             } deriving Show

parseTime :: ReadM Time
parseTime = eitherReader $ \s -> case s of
                (stripPrefix "now" -> Just rest) -> let offset = read rest
                                                    in Right $ Waiting offset (floor <$> POSIX.getPOSIXTime)
                time -> Right $ SetTime $ read time

parseCliArgs :: Parser CliArguments
parseCliArgs =
    let test = long "test"
            <> short 't'
            <> help "test the config file for syntax"
        evalRepl = long "evalRepl"
                <> short 'e'
                <> help "eval repl"
        evalOne = subparser $ command "query"
          ( info
            ( EvalOne
              <$> strOption (long "expr")
              <*> optional (option parseTime $ long "start")
              <*> optional (option parseTime $ long "end")
              <*> optional (option auto $ long "samples")
              <*> optional (strOption $ long "render")
            )
            ( progDesc "a" )
          )
        configfile = metavar "CONFIGFILE"
                  <> help "read config file from CONFIGFILE"
                  <> value "/etc/paradox/config"
        version = long "version"
               <> short 'v'
               <> help "show the version"
        fileArgument = argument str
    in CliArguments
       <$> optional evalOne
       <*> switch test
       <*> switch evalRepl
       <*> fileArgument configfile
       <*> switch version

replLoop :: MVar PT.ServerState -> Net.Socket -> IO ()
replLoop state sock = do
    conn <- Net.accept sock
    void $ forkIO $ replServe state conn
    replLoop state sock

replServe :: MVar PT.ServerState -> (SIO.Handle, Net.HostName, Net.PortNumber) -> IO ()
replServe st (handle, _, _) = do
    SIO.hSetBuffering handle SIO.NoBuffering
    void $ runReplWithHandle handle st



main :: IO ()
main = do
    setUncaughtExceptionHandler $ uncaughtExceptionHandler "toplevel" LG.logErrorG
    realState <- runOpts

    state <- newMVar realState

    let conf = PT.ssConfig realState
        port = PT.cPort conf

    void $ PSig.installHandler PSig.sigHUP (PSig.Catch (void $ modifyMVar state HL.renewLogs)) Nothing

    PL.startKeysQuery state
    logInfo (PT.ssConfig realState) $ LogText $ "Starting Paradox. "
                                             <> TS.pack (show versionString)
    logInfo (PT.ssConfig realState) $ LogText $ "Listening on port "
                                             <> TS.pack (show port)

    requestLogger <- liftIO $ mkRequestLogger $
        def { destination = Callback (LG.logToSpecificG "access" . LogText . TSE.decodeUtf8 . fromLogStr)
            , outputFormat = Apache FromSocket
            }

    void $ forkIO $ Net.withSocketsDo $ do
        sock <- Net.listenOn $ PT.cReplPort conf
        replLoop state sock

    WST.scottyT port (runActionIntoIO state) $ Routes.routes conf requestLogger
