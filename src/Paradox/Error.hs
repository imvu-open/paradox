{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Paradox.Error
( module Paradox.Error
, module Paradox.Error.Types
)
where

import Paradox.Error.Types
import Control.Monad.IO.Class

import Control.Exception                    (SomeException(..))
import Network.HTTP.Types           (status500)

import Paradox.Types                (RActionM)
import Data.Monoid                          ((<>))
import Paradox.Logger               (Log(..))

import qualified Web.Scotty.Trans   as WST
import qualified Paradox.Logger.Global      as LG
import qualified Data.Text                  as TS

handleEx :: Except -> RActionM ()
handleEx e@(TextError t) = do
    liftIO $ handledExceptionLogger e
    WST.status status500
    WST.json MkError { eData = Nothing :: Maybe String, eMsg = t }
handleEx e@(BackendKeysError innerEs) = do
    liftIO $ handledExceptionLogger e
    WST.status status500
    WST.json MkError { eData = Just innerEs, eMsg = "Backend server key fetch failure" }

handledExceptionLogger :: ToErrorMsg a => a -> IO ()
handledExceptionLogger !e =
    LG.logErrorG $ LogText $ "handled runtime error:"
                     <> " --error: " <> toErrorMsg e
                     <> "\n"

uncaughtExceptionHandler :: TS.Text -> (Log -> IO ()) -> SomeException -> IO ()
uncaughtExceptionHandler !prefix !output !e =
    output  $ LogText $ "unhandled runtime error [" <> prefix <> "]:"
                     <> " --error: " <> (TS.pack . show $ e)
                     <> "\n"
