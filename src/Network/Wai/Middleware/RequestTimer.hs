{-# LANGUAGE OverloadedStrings #-}
{- Found at http://stackoverflow.com/a/26146218 -}
module Network.Wai.Middleware.RequestTimer (
    requestTimer,
    ) where

import Prelude
import Network.Wai
import Data.Time (getCurrentTime, diffUTCTime)
import Paradox.Logger (Log(..))
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TSE

requestTimer :: (Log -> IO ()) -> Middleware
requestTimer logger app req sendResponse = do
    t0 <- getCurrentTime
    app req $ \rsp -> do
        t1 <- getCurrentTime
        logger $ LogText $ TS.concat ["The request [", TSE.decodeUtf8 $ rawPathInfo req, "] time is ", TS.pack $ show $ diffUTCTime t1 t0]
        sendResponse rsp
