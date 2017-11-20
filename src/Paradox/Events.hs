{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Paradox.Events (
    getAllEvents
) where

import Paradox.Events.Types

import Control.Exception                    (catch)

import Data.List                            (nub)
import Data.Foldable                        ( foldl' )
import Data.Traversable                     (forM)
import Paradox.Util                         ( foldl1' )
import Paradox.Istatd.Types                 ( EventReq(..)
                                            , TimeRange(..)
                                            , TimeWrapper(..)
                                            )
import Paradox.Logger.Global                ( logTraceG
                                            , Log(..)
                                            )

import qualified Paradox.Types              as PT
import qualified Network.HTTP.Client        as Client
import qualified Data.Text                  as TS
import qualified Data.Text.Encoding         as TSE
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Char8      as BSC
import qualified Data.Aeson                 as JSON
import qualified Data.Time.Clock.POSIX      as POSIX
import qualified Data.Map.Strict            as M


getEvents :: Client.Manager
          -> PT.EventConfig
          -> [EventReq]
          -> TimeRange
          -> IO (Either String [EventData])
getEvents manager eventConf events range = do
    let timeToInt :: TimeWrapper -> Integer
        timeToInt = floor .
                    POSIX.utcTimeToPOSIXSeconds .
                    unTimeWrapper

        intToBS :: Integer -> BSC.ByteString
        intToBS = BSC.pack .
                  show

        timeToBS :: TimeWrapper -> BSC.ByteString
        timeToBS = intToBS .
                   timeToInt

        uniqK :: [EventReq] -> BSC.ByteString
        uniqK = TSE.encodeUtf8 .
                TS.intercalate "," .
                nub .
                concatMap erKeys

        qs = [ ("events", Just $ uniqK events)
             , ("start", Just . timeToBS $ trStart range)
             , ("end", Just . timeToBS $ trStop range)
             ]

        target = PT.eventConfigToUrl eventConf
        req = Client.parseUrlThrow target
        req' = fmap (\r -> (Client.setQueryString qs r) { Client.responseTimeout = Client.responseTimeoutMicro 10000000 } ) req

        maybeWith a b c = maybe b c a

        handleEx :: Client.HttpException -> IO (Either String [EventData])
        handleEx ex = return $ Left $ show ex

    logTraceG $ LogText $ TS.pack $ show req'
    maybeWith
        req'
        (return $ Left "Url Couldn't be parsed")
        (\req'' ->
            Client.withResponse req'' manager $ \res -> do
                respBody <- BSL.fromChunks <$> Client.brConsume (Client.responseBody res)

                return $ unEventEnvelope <$> JSON.eitherDecode respBody) `catch` handleEx

getAllEvents :: Client.Manager
             -> M.Map TS.Text PT.EventConfig
             -> [EventReq]
             -> TimeRange
             -> IO (M.Map TS.Text [EventData])
getAllEvents manager evConfig events tr = do
    eventMaps :: M.Map TS.Text (M.Map TS.Text [EventData]) <-
        forM evConfig $ \event -> do
            let eFilter = PT.ecFilter event
                eventsPassFilter =
                    foldl' (\a e ->
                        if eFilter == erIdent e
                            then e:a
                            else a)
                        []
                        events

            case eventsPassFilter of
                [] -> return mempty
                allEvents -> do
                    evs <- getEvents manager event allEvents tr

                    let toPair x@EventData { message } = (message, [x])
                        pairs =
                            either
                                (const [])
                                (map toPair)
                                evs

                    return $ M.fromListWith (++) pairs

    return $ foldl1' (M.unionWith (++)) $ M.elems eventMaps
