{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Paradox.Handlers.Istatd where

import Control.Concurrent.STM
import Control.Concurrent.MVar
import "lifted-base" Control.Exception.Lifted
import Control.Monad.IO.Class

import Paradox.Eval
import Paradox.Logger

import Paradox.Debug

import Control.Arrow                            ((&&&))
import Network.HTTP.Types                       ( status200
                                                , status500
                                                , status400
                                                , status404
                                                )
import Network.HTTP.Types.Status                ( Status
                                                , mkStatus
                                                )
import Control.Monad                            ( when
                                                , void
                                                , forM
                                                , join
                                                )
import Data.Foldable                            (forM_)
import Control.Concurrent                       ( forkIO
                                                , threadDelay
                                                )
import Control.Concurrent.Async                 ( withAsync
                                                , wait
                                                )
import Data.Maybe                               ( isJust
                                                , fromJust
                                                , catMaybes
                                                )
import Data.Char                                (toLower)
import Data.Semigroup                           ((<>))
import Data.String                              (fromString)
import Data.Aeson                               ( ToJSON(..)
                                                , (.=)
                                                , object
                                                )
import Text.Blaze.Html.Renderer.Text            (renderHtml)
import Network.Mail.Mime                        ( simpleMailInMemory
                                                , Address(..)
                                                , Part(..)
                                                , Encoding(..)
                                                , addPart
                                                )
import Network.Mail.SMTP                        (sendMail)

import Language.Paradox.Eval.Types              ( Accum (..)
                                                , LogEntry (..)
                                                )

import Paradox.Error                            (Except(..))
import Imvu.Network.IstatdClient                (nameFromBytes)
import Paradox.Types                            (RActionM)

import Paradox.Logger.Global                    (logInfoG)
import Paradox.Render                           ( SvgResult(..)
                                                , mkSvgResult
                                                )
import Paradox.Istatd.Functions                 (mkTimeRange)
import Paradox.State                            ( withState
                                                , getMutableState
                                                , getMutableState'
                                                )

import System.Directory                         ( doesFileExist
                                                , removeFile
                                                , listDirectory
                                                , findFile
                                                )
import System.FilePath                          ( takeExtension
                                                , (</>)
                                                , takeFileName
                                                )

import qualified Paradox.Istatd.CounterTree     as CT
import qualified Web.Scotty.Trans               as WST
import qualified Paradox.Types                  as PT
import qualified Paradox.Logic                  as PL
import qualified Paradox.Istatd                 as I
import qualified Paradox.Istatd.Types           as IT
import qualified Paradox.Istatd.Util            as ITU
import qualified Paradox.Istatd.AsyncClient     as Istatd
import qualified Data.Aeson                     as JSON
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as TS
import qualified Data.Text.Encoding             as TSE
import qualified Data.Text.Lazy                 as TL
import qualified Data.Time.Clock.POSIX          as POSIX
import qualified Network.HTTP.Client.Internal   as ClientT
import qualified Data.Map.Strict                as M
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.ByteString.Lazy.Char8     as BSLC
import qualified Data.ByteString.Base64         as B64

import qualified Text.Blaze.Html5               as BLH
import qualified Text.Blaze.Html5.Attributes    as BLA

newtype SimpleErrorResponse = SimpleErrorResponse {
        unSimpleErrorResponse :: BSL.ByteString
    }

instance ToJSON SimpleErrorResponse where
    toJSON SimpleErrorResponse {..} = object [
          "status" .= ("error" :: String)
        , "message" .= BSLC.unpack unSimpleErrorResponse
        ]

data Render = PNG
            | SVG
            | PNGAndSVG
            deriving (Eq)
data Reply = Raw
           | JSON
           deriving Eq

replyFailedQueries :: QueryStatus
                   -> RActionM ()
replyFailedQueries = \case
    QuerySuccess {} -> return ()
    QueryUnresolvedSuccess {} -> return ()
    q -> do
        WST.status status422
        WST.json q

replyFailedKeys :: KeysStatus
                -> RActionM ()
replyFailedKeys = \case
    (KeysBackendError e) -> WST.raise $ BackendKeysError e
    _ -> return ()

timeIO :: String
       -> RActionM ()
       -> RActionM ()
timeIO s a = bracket
             startRequest
             endRequest
             (const a)
    where
        msg = fromString (map toLower s)
        startRequest = do
            withState $ \st -> do
                st' <- liftIO $ readMVar st
                liftIO . void . PT.ssIstatdLogger st' $
                  Istatd.Counter $ "endpoint" <> msg <> "hit"
            liftIO $ logStartRequest ("Start: " ++ s)
        endRequest t = do
            d <- liftIO $ logEndRequest ("End: " ++ s) t
            withState $ \st -> do
                st' <- liftIO $ readMVar st
                liftIO . void . PT.ssIstatdLogger st' $
                  Istatd.Counter $ "endpoint" <> msg <> "complete"
                liftIO . void . PT.ssIstatdLogger st' $
                  Istatd.Gauge ("endpoint" <> msg <> "time") (realToFrac d)

validateTemplate
  :: FilePath
  -> IT.Template
  -> Maybe IT.Template
validateTemplate filePath t@IT.Template {..} =
  let
    fileName = takeFileName filePath
  in
    if TS.pack fileName == tName
      then Just t
      else Nothing

getTemplates :: RActionM ()
getTemplates = timeIO "getTemplates" $ do
  templateDir <- getMutableState' (PT.cTemplateDir . PT.ssConfig)

  templates <- liftIO $ do
    files <- map (\t -> templateDir </> t) <$> listDirectory templateDir
    templateList <- catMaybes
      <$> forM files
               (\file -> do
                  template <- JSON.decode <$> BSLC.readFile file
                  let template' = validateTemplate file <$> template
                  return $ join template'
               )
    return $ HM.fromList $ map (\t -> (IT.tName t, t)) templateList
  WST.json templates

getTemplate :: RActionM ()
getTemplate = timeIO "getTemplate" $ do
  path :: String <- WST.param "name"

  templateDir <- getMutableState' (PT.cTemplateDir . PT.ssConfig)
  conf <- getMutableState' PT.ssConfig

  file <- liftIO $ findFile [templateDir] path
  case file of
    Just file' -> do
      logInfoM conf $ LogText $ TS.pack file'
      template <- JSON.decode <$> liftIO (BSLC.readFile file')
      case template of
        Just t@IT.Template {..} ->
          WST.json $ HM.singleton tName t
        Nothing -> do
          WST.status status500
          WST.json $ M.fromList [ (TS.pack "error", TS.pack "Template could not decode") ]
    Nothing -> do
      WST.status status404
      WST.json $ M.fromList [ (TS.pack "error", TS.pack "File not found") ]

passThruResponse :: ClientT.Response BSLC.ByteString
                 -> RActionM ()
passThruResponse res = do
    WST.status $ ClientT.responseStatus res
    WST.addHeader "Content-Type" "application/json"
    WST.raw $ ClientT.responseBody res

getPassThru :: RActionM ()
getPassThru = timeIO "getSettings" $ do
    params <- WST.params

    (manager, conf) <- getMutableState' (PT.ssManager &&& PT.ssConfig)
    res <- liftIO $ I.getSettings manager conf params
    passThruResponse res

postPassThru :: RActionM ()
postPassThru = timeIO "postSettings" $ do
    params <- WST.params
    body <- WST.body

    (manager, conf) <- getMutableState' (PT.ssManager &&& PT.ssConfig)
    res <- liftIO $ I.postSettings manager conf params body
    passThruResponse res

getParadoxCounters :: RActionM ()
getParadoxCounters = timeIO "getCounterTree" $ do
    pattern' <- WST.param "pattern"

    (manager, conf) <- getMutableState' (PT.ssManager &&& PT.ssConfig)

    res <- liftIO $ I.getCounters manager conf pattern'
    let tree = ITU.countersReplyToCounterTree res
    when (pattern' == "*") $ do
        withState $ \s -> liftIO . void . forkIO $
          PL.updateCounterMap s (ITU.countersReplyToCounterMap res) (IT.crTypeMapping res)
        withState $ \s -> liftIO . void . forkIO $
          PL.updateCounterTree s tree (IT.crTypeMapping res)
    WST.status status200
    let reply = IT.ParadoxCountersReply (IT.crTypeMapping res) pattern' tree
    WST.json reply

getParadoxCountersCached :: RActionM ()
getParadoxCountersCached = timeIO "getCounterTreeCached" $ do
    pattern' <- WST.param "pattern"

    (cacheState, conf) <- getMutableState' (PT.ssCacheState &&& PT.ssConfig)

    ctc <- liftIO $ atomically $ readTMVar $ PT.csCounterTreeCache cacheState

    if isJust $ PT.ctcTimeStamp ctc
        then fetchCachedCountersFromTree pattern'
        else do
            logInfoM conf $ LogText "Counters timestamp is too old refetch counters"
            getParadoxCounters

fetchCachedCountersFromTree :: TS.Text
                            -> RActionM ()
fetchCachedCountersFromTree pattern' =  do
    (cacheState, conf) <- getMutableState' (PT.ssCacheState &&& PT.ssConfig)

    ctc <- liftIO $ atomically $ readTMVar $ PT.csCounterTreeCache cacheState
    let cm = PT.ctcCounterTree ctc
    case CT.lookupManyT [pattern'] cm of
        Nothing -> do
            WST.status status500
            WST.addHeader "Content-Type" "application/json"
            WST.raw "Error"

        Just (_, !result) -> do
            let reply = IT.ParadoxCountersReply (PT.ctcTypeMapping ctc) pattern' result
            logInfoM conf $ LogText "Doing it cached from tree"
            WST.status status200
            WST.json reply

evalShared :: IT.ParadoxQuery
           -> PT.ServerState
           -> IO EvalStatus
evalShared pq state' = do
    typedQueriesStatus <- liftIO $ typeCheckQueriesAndResolve state' pq
    case guardOnQueries typedQueriesStatus of
        Nothing -> return $ EvalFailed $ replyFailedQueries typedQueriesStatus
        Just typedQueries -> do
            let qb = calculateQueryBounds pq typedQueries
                events = IT.pqEvents pq
            keysStatus <- liftIO $ fetchCountersForEval state' typedQueries qb
            eventData <- liftIO $ traceIt "Fetching Events"
                                      <$> fetchEventsForEval state'
                                                             events
                                                             qb
            case guardOnKeys keysStatus of
                Nothing -> return $ EvalFailed $ replyFailedKeys keysStatus
                Just keys -> return $ eval typedQueries keys eventData qb

logAccumToIstatd :: EvalResult
                 -> RActionM ()
logAccumToIstatd EvalResult {..} = do
    let Accum {..} = accumLogs
    istatdLogger <- getMutableState' PT.ssIstatdLogger

    forM_ unLogs $ \element -> do
        let ctr = nameFromBytes $ logEntry element
        forM_ ctr $ \ctr' ->
            liftIO $ istatdLogger $ Istatd.Counter $ "per_request"
                                                  <> "expression"
                                                  <> "element"
                                                  <> ctr'

    let getExprType e = filter ((==) e . exprType)
        countExprType e ls = length $ getExprType e ls
        (funcs, binds, lits, passs, highers) = fromJust $ convertToTuple $ map (`countExprType` unLogs)
                                                 [minBound..maxBound]
        convertToTuple it = case it of
            [f,b,l,p,h] -> Just (f,b,l,p,h)
            _ -> Nothing
        gauge name = Istatd.Gauge ("per_request" <> "expression" <> name)
        exprs = [ ("all", funcs + binds + lits + passs + highers)
                , ("lits", lits)
                , ("binds", binds)
                , ("funcs", funcs)
                , ("passthrus", passs)
                , ("higher_orders", highers)
                ]
    forM_ exprs $ \(name, count) ->
        liftIO $ istatdLogger $ gauge (name <> "count") (fromIntegral count)

    liftIO $ istatdLogger $ Istatd.Gauge ("per_request" <> "num_queries")
                                         (fromIntegral numQueries)

    let perQueryGauge name = Istatd.Gauge ("per_query" <> "expression" <> name)

    liftIO $ istatdLogger $ perQueryGauge ("all" <> "avg_count")
                                          exprPartsPerQuery
    liftIO $ istatdLogger $ perQueryGauge ("funcs" <> "avg_count")
                                          funcPerQuery
    liftIO $ istatdLogger $ perQueryGauge ("binds" <> "avg_count")
                                          bindsPerQuery
    liftIO $ istatdLogger $ perQueryGauge ("lits" <> "avg_count")
                                          litsPerQuery
    liftIO $ istatdLogger $ perQueryGauge ("passthrus" <> "avg_count")
                                          passesPerQuery
    liftIO $ istatdLogger $ perQueryGauge ("higher_orders" <> "avg_count")
                                          higherOrderPerQuery

getEval :: RActionM ()
getEval = timeIO "getEval" $ do
    query :: BSL.ByteString <- WST.param "query"

    let v' = JSON.decode query :: Maybe IT.ParadoxQuery
    state' <- getMutableState

    case v' of
        Just e -> do
            evalStatus <- liftIO $ evalShared e state'
            case evalStatus of
                EvalFailed act -> act
                EvalSuccess res accum -> do
                    logAccumToIstatd accum
                    WST.status status200
                    WST.json res
        Nothing -> do
            WST.status status400
            WST.json $ M.fromList [ (TS.pack "json", TS.pack "Malformed json") ]

postEval :: RActionM ()
postEval = timeIO "postEval" $ do
    b' <- WST.body
    let v' = JSON.decode b' :: Maybe IT.ParadoxQuery
    state' <- getMutableState

    case v' of
        Just e -> do
            evalStatus <- liftIO $ evalShared e state'
            case evalStatus of
                EvalFailed act -> act
                EvalSuccess res accum -> do
                    logAccumToIstatd accum
                    WST.status status200
                    WST.json res
        Nothing -> do
            WST.status status400
            WST.json $ M.fromList [ (TS.pack "json", TS.pack "Malformed json") ]

awaitPath :: String
          -> String
awaitPath path = path ++ ".await"

resultToResponse :: Render
                 -> Maybe SvgResult
                 -> RActionM ()
resultToResponse _ Nothing = return ()
resultToResponse render (Just svgS) = do
    (base, baseUrl) <- getMutableState' (PT.cPermalinkDir . PT.ssConfig &&& PT.cBaseUrl . PT.ssConfig)

    now <- liftIO POSIX.getPOSIXTime

    pnln <- if render /= SVG
               then do
                    let ext = ".png"
                        link = baseUrl ++ "permalink/" ++ show now ++ ext
                    writeAsync (`writePng` svgS) base ext now "Saving PNG in Fork"
                    return [(TS.pack "pngLink", TS.pack link)]
               else return []
    svln <- if render /= PNG
               then do
                    let ext = ".svg"
                        link = baseUrl ++ "permalink/" ++ show now ++ ext
                    writeAsync (`writeSvg` svgS) base ext now "Saving SVG in Fork"
                    return [(TS.pack "svgLink", TS.pack link)]
               else return []

    WST.status status200
    WST.json $ M.fromList (pnln ++ svln)


    where
        writeAsync :: MonadIO m
                   => (FilePath -> IO ())
                   -> FilePath
                   -> FilePath
                   -> POSIX.POSIXTime
                   -> String
                   -> m ()
        writeAsync writer base ext now msg = do
                    let path =  target base now ext
                        await = awaitPath path
                    liftIO $ do
                       writeToFile await ""
                       void . forkIO $ timeIt msg (writer path `finally` cleanup await)

        target :: FilePath
               -> POSIX.POSIXTime
               -> FilePath
               -> FilePath
        target b t ext = b ++ show t ++ ext

        writePng :: FilePath
                 -> SvgResult
                 -> IO ()
        writePng path SvgResult { toPng } = toPng $ \contents -> case contents of
                                                                       Just c -> writeToFile path c
                                                                       Nothing -> return ()

        writeSvg :: FilePath
                 -> SvgResult
                 -> IO ()
        writeSvg path SvgResult { toBytes } = toBytes $ \contents -> writeToFile path contents

        writeToFile :: FilePath
                    -> BSLC.ByteString
                    -> IO ()
        writeToFile = BSLC.writeFile

        cleanup :: FilePath
                -> IO ()
        cleanup = removeFile

evalSvg :: IT.ParadoxRenderQuery
        -> RActionM (Maybe SvgResult)
evalSvg prq@IT.ParadoxRenderQuery {..} = do
    state' <- getMutableState
    (conf, tzs) <- getMutableState' (PT.ssConfig &&& PT.ssTimeZoneSeries)
    let allKeys = concatMap IT.gdKeys $ M.elems prqGraphs
        pq = IT.ParadoxQuery prqRange allKeys [] prqMaxSamples
    evalStatus <- liftIO $ evalShared pq state'
    case evalStatus of
        EvalFailed act -> do
            logErrorM conf $ LogText $ "Failed to evaluate query from request\n"
                                    <> (TS.pack . show $ prq)
            act
            return Nothing
        EvalSuccess res accum -> do
            logAccumToIstatd accum
            fontSelector <- getMutableState' (PT.cFontsSelector . PT.ssConfig)
            fontSelector' <- liftIO $ readMVar fontSelector
            svg <- liftIO $ mkSvgResult fontSelector'
                                        tzs
                                        prqGraphs
                                        res
                                        prqWidth
                                        prqHeight

            return $ Just svg


createPermalink :: RActionM ()
createPermalink = timeIO "createPermalink" $ do
    b' <- WST.body
    let v' = JSON.eitherDecode b' :: Either String IT.ParadoxRenderQuery
    case v' of
        Right e -> do
            svgS <- evalSvg e

            resultToResponse PNGAndSVG svgS
        Left e -> do
            WST.status status400
            WST.json $ M.fromList [ (TS.pack "json", TS.pack $ "Malformed json " ++ show e) ]

fetchPermalink :: RActionM ()
fetchPermalink = timeIO "fetchPermalink" $ do
    path :: String <- WST.param "path"

    (base, waitTimeout) <- getMutableState' (PT.cPermalinkDir . PT.ssConfig &&& PT.cPermalinkWaitTimeout . PT.ssConfig)
    conf <- getMutableState' PT.ssConfig

    let target = base ++ path
    exist <- liftIO $ doesFileExist target
    if exist
        then openIt conf target
        else do
            let await = awaitPath target
                oneHundredMS :: Integral b => b
                oneHundredMS = 100000
            let waitExists :: MonadIO m
                           => Integer
                           -> m ()
                waitExists !acc = do
                    does <- liftIO $ doesFileExist await
                    when (does && acc > 0) $ do
                        liftIO $ threadDelay oneHundredMS
                        waitExists (acc - oneHundredMS)
            waitExists waitTimeout
            exist' <- liftIO $ doesFileExist target
            if exist'
                then openIt conf target
                else do
                    WST.status status400
                    WST.json $ M.fromList [ (TS.pack "json", TS.pack $ "svg link doesnt exist" ++ show path) ]
    where
        openIt conf target = case takeExtension target of
            ('.':ext) -> do
                svg <- liftIO $ BSLC.readFile target

                WST.status status200
                WST.setHeader "Content-Type" (TL.concat ["image/", TL.pack ext])
                WST.raw svg
            _ -> logWarningM conf $ LogText (TS.concat ["Could not get an extension from target: ", TS.pack target])

evalAndRender :: Render
              -> Reply
              -> IT.ParadoxRenderQuery
              -> RActionM ()
evalAndRender render reply prq@IT.ParadoxRenderQuery {..} = do
    when (reply == JSON && prqInlineHTML) $ do
        WST.status status400
        WST.json $ M.fromList [ (TS.pack "success", TS.pack "false"), (TS.pack "error", TS.pack "Incompatible arguments") ]
        fail "Incompatible Arguments"
    when (render == PNGAndSVG && not prqInlineHTML && reply /= JSON) $ do
        WST.status status400
        WST.json $ M.fromList [ (TS.pack "success", TS.pack "false"), (TS.pack "error", TS.pack "Incompatible arguments") ]
        fail "Incompatible Arguments"

    conf <- getMutableState' PT.ssConfig
    svg <- evalSvg prq

    forM_ svg $ \svg' ->
        toPngAndBytes svg' $ \pngBytesM svgBytes -> do
            let pngBytes = fromJust pngBytesM
            if prqInlineHTML
               then do
                   let svgImg =
                           BLH.img BLH.!
                               BLA.src (BLH.toValue $
                               TS.concat [ "data:image/svg+xml;base64,"
                                           , TSE.decodeUtf8 $
                                           B64.encode $
                                           BSLC.toStrict svgBytes
                                           ]
                                       )
                       pngImg =
                           BLH.img BLH.!
                               BLA.src (BLH.toValue $
                               TS.concat [ "data:image/png;base64,"
                                           , TSE.decodeUtf8 $
                                           B64.encode $
                                           BSLC.toStrict pngBytes
                                           ]
                                       )
                       html = case render of
                                SVG -> svgImg
                                PNG -> pngImg
                                PNGAndSVG -> do
                                    svgImg
                                    pngImg
                   WST.html . renderHtml $
                            BLH.html $
                                BLH.body html
                   WST.status status200
                   WST.setHeader "Content-Type" "text/html"
               else do
                    when (render == SVG && reply /= JSON) $ do
                        WST.raw svgBytes
                        WST.status status200
                        WST.setHeader "Content-Type" "image/svg+xml"
                    when (render == PNG && reply /= JSON) $ do
                        WST.raw pngBytes
                        WST.status status200
                        WST.setHeader "Content-Type" "image/png"

            forM_ prqEmailTo $ \email -> do
                (smtpServer, sourceAddr) <- getMutableState'
                    ( PT.cSMTPServer . PT.ssConfig &&& PT.cSourceAddr . PT.ssConfig)
                forM_ smtpServer $ \smtp ->
                    forM_ sourceAddr $ \source -> do
                        logDebugM conf $ LogText "Email construction"
                        let html =
                                let pngDiv = BLH.div BLH.! BLA.dir "ltr" $ BLH.img BLH.! BLA.src "cid:paradoxpng"
                                    svgDiv = BLH.div BLH.! BLA.dir "ltr" $ BLH.img BLH.! BLA.src "cid:paradoxsvg"
                                in case render of
                                  PNG -> pngDiv
                                  SVG -> svgDiv
                                  PNGAndSVG -> do
                                      pngDiv
                                      svgDiv
                            mail'' = simpleMailInMemory
                                (Address Nothing email)
                                (Address Nothing source)
                                "Paradox"
                                ""
                                (renderHtml html)
                                []

                            mail' = let part =
                                            let pngPart = Part "image/png"
                                                               Base64
                                                               Nothing
                                                               [ ("Content-Disposition", "inline; filename=\"paradox.png\"")
                                                               , ("Content-ID","<paradoxpng>")
                                                               , ("X-Attachment-Id", "paradoxpng")
                                                               ]
                                                               pngBytes
                                                svgPart = Part "image/svg+xml"
                                                               Base64
                                                               Nothing
                                                               [ ("Content-Disposition", "inline; filename=\"paradox.svg\"")
                                                               , ("Content-ID","<paradoxsvg>")
                                                               , ("X-Attachment-Id", "paradoxsvg")
                                                               ]
                                                               svgBytes
                                            in case render of
                                              PNG -> [pngPart]
                                              SVG -> [svgPart]
                                              PNGAndSVG -> [pngPart, svgPart]
                                    in addPart part mail''
                            mail = let part =
                                            let pngPart = Part "image/png"
                                                               Base64
                                                               (Just "paradox.png")
                                                               [ ("Content-ID","<paradoxpng2>")
                                                               , ("X-Attachment-Id", "paradoxpng2")
                                                               ]
                                                               pngBytes
                                                svgPart = Part "image/svg+xml"
                                                               Base64
                                                               (Just "paradox.svg")
                                                               [ ("Content-ID","<paradoxsvg2>")
                                                               , ("X-Attachment-Id", "paradoxsvg2")
                                                               ]
                                                               svgBytes
                                            in case render of
                                              PNG -> [pngPart]
                                              SVG -> [svgPart]
                                              PNGAndSVG -> [pngPart, svgPart]
                                   in addPart part mail'

                        let innerSendMail = do
                               logDebug conf $ LogText "Sending mail asynchronously"
                               sendMail smtp mail
                               logDebug conf $ LogText "After sending mail asynchronously"

                        liftIO $ withAsync innerSendMail wait

            when (reply == JSON) $ do
                WST.status status200
                WST.json $ M.fromList [ (TS.pack "success", TS.pack "true") ]

postEvalAndRender :: RActionM ()
postEvalAndRender = timeIO "postEvalAndRender" $ do
    b' <- WST.body
    let v' = JSON.eitherDecode b' :: Either String IT.ParadoxRenderQuery
    case v' of
        Right e -> evalAndRender SVG JSON e
        Left e -> do
            WST.status status400
            WST.json $ M.fromList [ (TS.pack "json", TS.pack $ "Malformed json " ++ show e) ]

postEvalAndRenderPng :: RActionM ()
postEvalAndRenderPng = timeIO "postEvalAndRenderPng" $ do
    b' <- WST.body
    let v' = JSON.eitherDecode b' :: Either String IT.ParadoxRenderQuery
    case v' of
        Right e -> evalAndRender PNG JSON e
        Left e -> do
            WST.status status400
            WST.json $ M.fromList [ (TS.pack "json", TS.pack $ "Malformed json " ++ show e) ]

collectRenderParameters :: RActionM IT.ParadoxRenderQuery
collectRenderParameters = do
    let defaultArg e v = e `WST.rescue` (const . return $ v)

    start :: Integer <- WST.param "start"
    stop :: Integer <- WST.param "stop"
    prqMaxSamples :: Int <- WST.param "maxSamples"
    prqWidth :: Double <- WST.param "width" `defaultArg` 800.0
    prqHeight :: Double <- WST.param "height" `defaultArg` 600.0
    prqEmailTo :: Maybe TS.Text <- WST.param "email" `defaultArg` Nothing
    prqInlineHTML :: Bool <- WST.param "inline_html" `defaultArg` False

    let usedKeys = [ "start"
                   , "stop"
                   , "maxSamples"
                   , "width"
                   , "height"
                   , "email"
                   , "inline_html"
                   ]

    graphs <- filter (\(k,_) -> notElem k usedKeys) <$> WST.params

    let parseGraphs k v = (TL.toStrict k, map IT.textToCExpr $ read (TL.unpack v))
        graphs' = map (uncurry parseGraphs) graphs
        prqRange = mkTimeRange start stop
        prqGraphs = M.map (`IT.GraphData` M.empty) $ M.fromList graphs'
        prq = IT.ParadoxRenderQuery {..}

    return prq

getEvalAndRender :: RActionM ()
getEvalAndRender = timeIO "getEvalAndRender" $ do
    prq <- collectRenderParameters

    evalAndRender SVG Raw prq

getEvalAndRenderPng :: RActionM ()
getEvalAndRenderPng = timeIO "getEvalAndRenderPng" $ do
    prq <- collectRenderParameters

    evalAndRender PNG Raw prq


postEvalAndEmail :: RActionM ()
postEvalAndEmail = timeIO "postEvalAndEmail" $ do
    b' <- WST.body
    let v' = JSON.eitherDecode b' :: Either String IT.ParadoxRenderQuery
    case v' of
        Right e -> evalAndRender SVG JSON e
        Left e -> do
            WST.status status400
            WST.json $ M.fromList [ (TS.pack "json", TS.pack $ "Malformed json " ++ show e) ]

getEvalAndEmailPng :: RActionM ()
getEvalAndEmailPng = timeIO "getEvalAndEmailPng" $ do
    prq <- collectRenderParameters

    evalAndRender PNG Raw prq

postEvalAndEmailPng :: RActionM ()
postEvalAndEmailPng = timeIO "postEvalAndEmailPng" $ do
    b' <- WST.body
    let v' = JSON.eitherDecode b' :: Either String IT.ParadoxRenderQuery
    case v' of
        Right e -> evalAndRender PNG JSON e
        Left e -> do
            WST.status status400
            WST.json $ M.fromList [ (TS.pack "json", TS.pack $ "Malformed json " ++ show e) ]

getEvalAndEmail :: RActionM ()
getEvalAndEmail = timeIO "getEvalAndEmail" $ do
    prq <- collectRenderParameters

    evalAndRender SVG Raw prq


status422 :: Status
status422 = mkStatus 422 "Unprocessable Entity"

logStartRequest :: String
                -> IO POSIX.POSIXTime
logStartRequest message = do
    time <- POSIX.getPOSIXTime
    logInfoG $ LogText $ TS.pack message
                      <> ": now: "
                      <> TS.pack (show time)
    return time

logEndRequest :: String
              -> POSIX.POSIXTime
              -> IO POSIX.POSIXTime
logEndRequest message startTime = do
    time <- POSIX.getPOSIXTime
    let diff = time - startTime
    logInfoG $ LogText $ TS.pack message
                      <> ": now: "
                      <> TS.pack (show time)
                      <> " time: "
                      <> TS.pack (show diff)
    return diff
