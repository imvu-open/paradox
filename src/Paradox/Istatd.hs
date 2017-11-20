{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Paradox.Istatd
( getSettings
, postSettings
, getCounters
, getCountersRaw
, batchGetStatsViaCacheCompactCE'
)
where

import Paradox.Debug
import Paradox.Cache
import qualified Paradox.Cache as Cache
import "lifted-base" Control.Exception.Lifted


import Data.Maybe                             (fromMaybe)
import Control.Concurrent.Async               (mapConcurrently)

import Paradox.Istatd.Functions               ( correctWithOffset
                                              , fromPostRequestToCompact
                                              , createPostRequest
                                              , shard, unshard
                                              , correctIntervals
                                              , removeEmpties
                                              , combinePostReplies
                                              )
import Paradox.Istatd.Types                   ( PostReplyRet (..)
                                              , CounterSpec (..)
                                              , CountersReply (..)
                                              , TimeRange (..)
                                              , PostReply (..)
                                              , PostReplyErrorBody (..)
                                              )

import Paradox.Istatd.Cache.Types             ( CountersReq(..))
import Paradox.Eval.Types                     (KeysStatus(..))
import qualified Paradox.Types                as PT
import qualified Data.Aeson                   as JSON
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy.Char8   as BSL
import qualified Network.HTTP.Client          as Client
import qualified Network.HTTP.Client.Internal as ClientT
import qualified Data.Text                    as TS
import qualified Data.Text.Encoding           as TSE
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TLE
import qualified Data.Map.Strict              as M
import qualified Data.List                    as L
import qualified Network.HTTP.Types.Header    as Header

import Paradox.Util (descriptiveHead)


parseResponse :: JSON.FromJSON a
              => ClientT.Response ClientT.BodyReader
              -> (Either String a -> BSL.ByteString -> b)
              -> IO b
parseResponse res resp = do
    respBody <- BSL.fromChunks <$> Client.brConsume (Client.responseBody res)

    let respStruct = JSON.eitherDecode respBody

    return $ resp respStruct respBody

basicHeaders :: ClientT.Request
             -> Header.RequestHeaders
basicHeaders initReq =
      ("Accept-Encoding", "")
    : ("Content-Type", "application/json")
    : ("Connection", "keep-alive")
    : ClientT.requestHeaders initReq


batchGetStatsViaCacheCompactCE' :: Client.Manager
                               -> PT.Configuration
                               -> CacheFn CountersReq PostReply
                               -> [CounterSpec]
                               -> TimeRange
                               -> Int
                               -> IO KeysStatus
batchGetStatsViaCacheCompactCE' manager conf purememofn keys csrTimeRange csrMaxSamples = do
    res <- timeIt "TotalTime getting batch of counters"
        $ batchGetStatsViaCacheCompactCE manager
                                       conf
                                       purememofn
                                       keys
                                       csrTimeRange
                                       csrMaxSamples
    let hasErrors = any (\x ->
            case x of
                PostReplyError {} -> True
                _                   -> False
                        ) res

    if hasErrors
        then do
            let errs = [f | PostReplyError f <- res]
            return $ KeysBackendError errs
        else do
            let replies = correctIntervals [(x, removeEmpties f) | PostReplySuccess x f <- res]
            return $ KeysSuccess replies


batchGetStatsViaCacheCompactCE :: Client.Manager
                              -> PT.Configuration
                              -> CacheFn CountersReq PostReply
                              -> [CounterSpec]
                              -> TimeRange
                              -> Int
                              -> IO [PostReplyRet]
batchGetStatsViaCacheCompactCE manager conf purememofn keys csrTimeRange csrMaxSamples = do
    let keyGroups = M.fromListWith (++) [(k, [v]) | v@(CounterSpec _ k) <- keys]
        memo csrKeys = getStatsViaCacheCompactCE manager conf purememofn CountersReq {..}

        istatdReqSize = PT.cIstatdReqSize conf

    L.concat . M.elems . unshard <$> mapConcurrently memo (shard istatdReqSize keyGroups)

getStatsViaCacheCompactCE :: Client.Manager
                         -> PT.Configuration
                         -> CacheFn CountersReq PostReply
                         -> CountersReq
                         -> IO PostReplyRet
getStatsViaCacheCompactCE manager conf pureMemoFn cr = do
    let subKeysFn key = map (\k -> cr { csrKeys = [k] }) (csrKeys key)
        missingKeysToKey [] = Nothing
        missingKeysToKey (k:r) = Just $ k { csrKeys = csrKeys k ++ concatMap csrKeys r }
        fko = csOffset $ descriptiveHead "couldnt get offset from keys" (csrKeys cr)
        valueToValues k v = M.elems $ M.mapWithKey (\k' v' -> (k { csrKeys = [CounterSpec k' fko] }, v { prKeys = M.fromList [(k', v')]})) (prKeys v)
        keyToResultF k = do
            req <- createPostRequest (csrKeys k) (csrTimeRange k) (csrMaxSamples k)
            let initReq = mkInitialRequestMGet conf
                bodyBS = JSON.encode $ fromPostRequestToCompact req True
                body = ClientT.RequestBodyLBS bodyBS
                headers = basicHeaders initReq
                initReq' = initReq {
                      ClientT.method = "POST"
                    , ClientT.requestHeaders = headers
                    , ClientT.requestBody = body
                    }
                msg = concat ["Requesting n: ", show (length $ csrKeys k), " keys"]
            timeIt msg $ Client.withResponse initReq' manager $ \res ->
                parseResponse res $ \respStruct respBody ->
                    case respStruct of
                        Right val ->
                            let firstKeyOffset = csOffset $ descriptiveHead "couldnt get offset from keys" (csrKeys k)
                                results = correctWithOffset firstKeyOffset val
                            in results
                        Left e ->
                            throw $ PostReplyErrorBody e respBody "Bad Reply from backend"


    catch
      (PostReplySuccess fko <$> Cache.cacheEx pureMemoFn cr subKeysFn missingKeysToKey keyToResultF valueToValues combinePostReplies)
      (return . PostReplyError)

addHostHeader :: PT.Configuration
              -> ClientT.Request
              -> ClientT.Request
addHostHeader conf initReq =
    let host = BSL.toStrict $ PT.getHost conf
        hostHeader = ("Host", host)
    in initReq { ClientT.requestHeaders = hostHeader:ClientT.requestHeaders initReq }

constructTarget :: PT.Configuration
                -> BSL.ByteString
                -> String
constructTarget conf path =
    let target = BSL.unpack target''
        target' = PT.getTarget conf
        target'' = BSL.append target' path
    in target

mkInitialAuthRequest :: PT.Configuration
                     -> BSL.ByteString
                     -> ClientT.Request
mkInitialAuthRequest conf path =
    let target = constructTarget conf path
        user = BSL.toStrict $ PT.getUser conf
        password = BSL.toStrict $ PT.getPassword conf
    in  Client.applyBasicAuth user password $ fromMaybe (error $ "Failed to parse url from: " ++ show target) $ Client.parseUrlThrow target


mkInitialSimpleRequest :: PT.Configuration
                       -> BSL.ByteString
                       -> ClientT.Request
mkInitialSimpleRequest conf path =
    let target = constructTarget conf path
    in fromMaybe (error $ "Failed to parse url from: " ++ show target) $ Client.parseUrlThrow target

mkInitialRequest :: PT.Configuration
                 -> BSL.ByteString
                 -> ClientT.Request
mkInitialRequest conf@(PT.getUseAuth -> True) path = go $ mkInitialAuthRequest conf path
    where
        go = addHostHeader conf
mkInitialRequest conf@(PT.getUseAuth -> False) path = go $ mkInitialSimpleRequest conf path
    where
        go = addHostHeader conf
mkInitialRequest _ _ = error "Satisfy -wWerror this case shouldnt happen since all we care about is getUseAuth Truthiness"

mkInitialRequest' :: PT.Configuration
                  -> ClientT.Request
mkInitialRequest' conf = mkInitialRequest conf ""

mkInitialRequestMGet :: PT.Configuration
                     -> ClientT.Request
mkInitialRequestMGet conf = mkInitialRequest conf "*"

getCounters :: Client.Manager
            -> PT.Configuration
            -> TS.Text
            -> IO CountersReply
getCounters manager conf globPattern = do
    let initReq = mkInitialRequest' conf
        initReq' = Client.setQueryString [("q",Just $ TSE.encodeUtf8 globPattern)] initReq
        headers = ("Connection", "keep-alive")
                  :ClientT.requestHeaders initReq'
        initReq'' = initReq' { ClientT.requestHeaders = headers }
        msg = "Counters get request to istatd and conversion:"
    timeIt msg $ Client.withResponse initReq'' manager $ \res ->
        parseResponse res $ \respStruct respBody ->
            case respStruct of
                Right val ->
                    val
                Left e ->
                    error $ concat ["Failed to get counters\n", show e, "\n", show respBody]

getCountersRaw :: Client.Manager
               -> PT.Configuration
               -> TS.Text
               -> IO (ClientT.Response BSL.ByteString)
getCountersRaw manager conf globPattern = do
    let initReq = mkInitialRequest' conf
        initReq' = Client.setQueryString [("q",Just $ TSE.encodeUtf8 globPattern)] initReq
        msg = "Counters get' request to istatd and conversion:"
    timeIt msg $ Client.withResponse initReq' manager $ \res -> do
        respBody <- BSL.fromChunks <$> Client.brConsume (Client.responseBody res)

        return res { ClientT.responseBody = respBody }

settingsArgs :: [(TL.Text, TL.Text)]
             -> [(BS.ByteString, Maybe BS.ByteString)]
settingsArgs args =
    let fixArgs = map (\(l,r) -> (BSL.toStrict $ TLE.encodeUtf8 l, Just $ BSL.toStrict $ TLE.encodeUtf8 r))
    in fixArgs args

getSettings :: Client.Manager
            -> PT.Configuration
            -> [(TL.Text, TL.Text)]
            -> IO (ClientT.Response BSL.ByteString)
getSettings manager conf args = do
    let initReq = mkInitialRequest' conf
        initReq' = initReq {
              ClientT.method = "GET"
            , Client.checkResponse = \_ _ -> pure ()
            }
        initReq'' = Client.setQueryString (settingsArgs args) initReq'
        msg = "Counters get' request to istatd and conversion:"

    timeIt msg $ Client.withResponse initReq'' manager $ \res -> do
        respBody <- BSL.fromChunks <$> Client.brConsume (Client.responseBody res)

        return res { ClientT.responseBody = respBody }

postSettings :: Client.Manager
             -> PT.Configuration
             -> [(TL.Text, TL.Text)]
             -> BSL.ByteString
             -> IO (ClientT.Response BSL.ByteString)
postSettings manager conf args body = do
    let initReq = mkInitialRequest' conf
        initReq' = initReq {
              ClientT.method = "POST"
            , ClientT.requestHeaders = ("Content-Type", "application/json"):ClientT.requestHeaders initReq
            , ClientT.requestBody = ClientT.RequestBodyLBS body
            , Client.checkResponse = \_ _ -> pure ()
            }
        initReq'' = Client.setQueryString (settingsArgs args) initReq'
        msg = "Counters get' request to istatd and conversion:"

    timeIt msg $ Client.withResponse initReq'' manager $ \res -> do
        respBody <- BSL.fromChunks <$> Client.brConsume (Client.responseBody res)

        return res { ClientT.responseBody = respBody }
