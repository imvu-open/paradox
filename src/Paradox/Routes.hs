{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Paradox.Routes (
  routes
) where

import Control.Monad.IO.Class

import Data.Maybe                                       (fromJust)
import Control.Arrow                                    ( second )
import Network.Wai.Middleware.Static                    ( addBase
                                                        , (<|>)
                                                        , (>->)
                                                        , contains
                                                        , only
                                                        , addSlash
                                                        , staticPolicy
                                                        )

import Network.Wai                                      (Middleware)
import Network.Wai.Middleware.Gzip                      ( gzip
                                                        , def
                                                        , gzipFiles
                                                        , GzipFiles( GzipCompress )
                                                        )
import Network.Wai.Middleware.RequestTimer              (requestTimer)
import Network.HTTP.Types                               (status200)
import Paradox.Types                                    ( RScottyM
                                                        , RActionM
                                                        , getServeBase
                                                        )
import Paradox.Error                                    (handleEx)
import Paradox.Logger.Global                            (logInfoG)
import Data.ByteString.Lazy.Char8                       (unpack)
import Data.Text.Lazy                                   (Text)
import Paradox.Istatd.CounterMap                        ( lookupMagic
                                                        , convertForSearch
                                                        , unpackSearch
                                                        , deMaybe
                                                        , make
                                                        )
import Paradox.State                                    ( getMutableState )
import Paradox.Istatd.Functions                         ( manyConvTSToPTS)
import Paradox.Eval.Types                               (KeysStatus(..))
import Paradox.Version                                  (versionString)

import qualified Paradox.Handlers.Istatd                as IR
import qualified Language.Paradox.Eval.Help             as H
import qualified Language.Paradox.Eval.TypeRep.Help     as H
import qualified Paradox.Handlers.Log                   as LR
import qualified Web.Scotty.Trans                       as WST
import qualified Paradox.Types                          as PT
import qualified Data.Text.Encoding                     as TSE
import qualified Data.Text                              as TS
import qualified Data.Text.Lazy                         as TL
import qualified Data.ByteString.Lazy                   as BSL
import qualified Paradox.Anomaly                        as Anomaly
import qualified Paradox.Istatd                         as I
import qualified Paradox.Istatd.Types                   as IT
import qualified Data.Time.Clock.POSIX                  as POSIX
import qualified Data.Map.Strict                        as M
import qualified Data.DTW                               as DTW

routes :: PT.Configuration
       -> Middleware
       -> RScottyM ()
routes conf requestLogger = do

    let base = unpack $ getServeBase conf
        base' = case base of
            "" -> "files"
            b -> concat [b, "/", "files"]


    WST.middleware $ requestTimer logInfoG
    WST.middleware requestLogger
    WST.middleware $ staticPolicy (
                addSlash >-> only [("/",concat [base', "/", "index.html"])]
            <|> contains "files" >-> addBase base
            <|> addBase base'
            )

    WST.middleware $ gzip def { gzipFiles = GzipCompress }

    WST.defaultHandler handleEx

    WST.post "/eval" IR.postEval
    WST.get "/eval" IR.getEval
    WST.get "/counters/:pattern" (checkCacheHeader IR.getParadoxCounters IR.getParadoxCountersCached)
    WST.get "/settings" IR.getPassThru
    WST.get "/templates" IR.getTemplates
    WST.get "/template/:name" IR.getTemplate
    WST.get "/log" LR.getLogConfig
    WST.get "/log/:name" LR.getLogConfigByName
    WST.post "/log_config/:name" LR.postLogConfigByName
    WST.post "/log_level/:name/:level" LR.postLogConfigByName'
    WST.post "/renew_logs/" LR.postRenewLogs
    WST.post "/settings" IR.postPassThru
    WST.post "/render" IR.postEvalAndRender
    WST.get "/render" IR.getEvalAndRender
    WST.post "/email" IR.postEvalAndEmail
    WST.get "/email" IR.getEvalAndEmail
    WST.post "/renderpng" IR.postEvalAndRenderPng
    WST.get "/renderpng" IR.getEvalAndRenderPng
    WST.post "/emailpng" IR.postEvalAndEmailPng
    WST.get "/emailpng" IR.getEvalAndEmailPng
    WST.post "/permalink" IR.createPermalink
    WST.get "/permalink/:path" IR.fetchPermalink
    WST.get "/help" $ do
        WST.status status200
        WST.json H.help
    WST.post "/function_help/type" $ do
        t <- WST.body
        WST.status status200
        WST.json $ H.findHelp $ TS.strip $ TSE.decodeUtf8 $ BSL.toStrict t
    WST.get "/compare/dtw" $ do
        left <- WST.param "left"
        right <- WST.param "right"
        start :: Integer <- WST.param "start"
        end :: Integer <- WST.param "end"
        let keys = map (`IT.CounterSpec` Nothing) [left,right]
            tr = IT.TimeRange (IT.TimeWrapper $ POSIX.posixSecondsToUTCTime $ fromIntegral start) (IT.TimeWrapper $ POSIX.posixSecondsToUTCTime $ fromIntegral end)
        state <- getMutableState

        keysStatus <- liftIO $ I.batchGetStatsViaCacheCompactCE' (PT.ssManager state) (PT.ssConfig state) (PT.ssCache state) keys tr 600
        case keysStatus of
            KeysSuccess replies -> do
                let m = make $ manyConvTSToPTS $ map (second IT.prnKeys) replies
                    g e = let r = fromJust $ M.lookup (IT.CounterSpec e Nothing) $ M.fromList $ unpackSearch $ deMaybe $ concatMap (\x -> lookupMagic (convertForSearch x) m) keys
                          in IT.ptsData r
                    res = Anomaly.invokeDTW (g left) (g right)
                WST.status status200
                WST.json $ M.fromList [("cost" :: TS.Text, DTW.cost res)]
            _ -> return ()

    WST.get "/healthcheck" $ do
        WST.status status200
        WST.text $ TL.pack $ unlines [ "Paradox is running"
                                     , versionString
                                     ]
    WST.get "/version" $ do
        WST.status status200
        WST.text $ TL.pack versionString

checkCacheHeader :: RActionM ()
                 -> RActionM ()
                 -> RActionM ()
checkCacheHeader noCache cached = do
    headerNoCache <- WST.header "Cache-Control"
    headerPragma <- WST.header "Pragma"

    let cacheOff :: Maybe Text
                 -> Maybe Text
                 -> Bool
        cacheOff h p = case p of
            Just "no-cache" -> True
            _ -> case h of
                Just "no-cache" -> True
                _ -> False

    if cacheOff headerNoCache headerPragma then
        noCache
    else
        cached
