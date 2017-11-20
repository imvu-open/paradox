{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Paradox.Events.Types where

import Paradox.Util                         (mapToAesonListFiltered)
import Data.Text                            (Text)
import Data.Map.Strict                      (Map)
import Data.Aeson                           ( object
                                            , ToJSON(..)
                                            , FromJSON(..)
                                            , withObject
                                            , withText
                                            , Value(..)
                                            , (.:)
                                            , (.=)
                                            )

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import qualified Data.HashMap.Strict        as H

data EventData = EventData
    { createdAt :: Integer
    , message :: Text
    , values :: Map Text Text
    } deriving (Show)

instance ToJSON EventData where
    toJSON EventData {..} = object $ [
              "created_at" .= createdAt
            , "message" .= message
        ] ++ keyv
        where
            keyv = mapToAesonListFiltered values ["message", "created_at"]
            --keyv = H.toList $ M.foldrWithKey (\k -> H.insert k . toJSON) H.empty
            --              $ M.filterWithKey (\x _ -> notElem x ["message","created_at"])
            --                                values

instance FromJSON EventData where
    parseJSON = withObject "FromJSON EventData" $ \o ->
              EventData <$> o .: "created_at"
                        <*> o .: "message"
                        <*> ( parseJSON
                            . Object
                            . H.filterWithKey (\k _ -> notElem k ["created_at", "message"])
                            ) o

newtype EventEnvelope = EventEnvelope { unEventEnvelope :: [EventData] }
                      deriving Show

instance FromJSON EventEnvelope where
    parseJSON = withObject "FromJSON EventWrapper" $ \o ->
      EventEnvelope <$> o .: "events"

newtype EventFilter = EventFilter BSL.ByteString
                    deriving (Show, Eq)

instance FromJSON EventFilter where
    parseJSON = withText "FromJSON EventFilter" $ \t ->
            let tl = TL.fromStrict t
                verify ti = TL.length ti > 1 && TL.isSuffixOf ":" ti
            in if verify tl
                  then return $ EventFilter (TLE.encodeUtf8 tl)
                  else fail $ "Failed to parse filter from " ++ show tl

