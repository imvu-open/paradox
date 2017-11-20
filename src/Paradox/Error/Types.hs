{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Paradox.Error.Types where

import Data.Monoid                  ((<>))
import Data.Typeable                (Typeable)
import Control.Exception                    (Exception)
import Data.String                  (fromString)
import Data.Aeson                   ( (.=)
                                    , toJSON
                                    , ToJSON
                                    , object
                                    )

import qualified Web.Scotty.Trans   as WST
import qualified Data.Text.Lazy     as TL
import qualified Data.Text          as TS
import qualified Data.ByteString.Lazy as BSL
import qualified Paradox.Istatd.Types as IT

data MkError a = MkError {
      eData :: Maybe a
    , eMsg :: TL.Text
    }

instance ToJSON a => ToJSON (MkError a) where
    toJSON MkError {..} = object [
          "status" .= TL.pack "error"
        , "data" .= eData
        , "message" .= eMsg
        ]

class ToErrorMsg a where
    toErrorMsg :: a -> TS.Text

data Except = TextError TL.Text | BackendKeysError [IT.PostReplyErrorBody]
    deriving (Show, Eq, Typeable)

instance ToErrorMsg Except where
    toErrorMsg (BackendKeysError es) = TS.pack .show $ BackendKeysError $ map (\(IT.PostReplyErrorBody e b m) -> IT.PostReplyErrorBody e (BSL.take 100 b <> "...") m) es
    toErrorMsg err = TS.pack . show $ err

instance Exception Except

instance WST.ScottyError Except where
    stringError = TextError . TL.pack
    showError = fromString . show


