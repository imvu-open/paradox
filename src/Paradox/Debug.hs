module Paradox.Debug
(
      traceIt
    , timeIt
    , trace
) where

import Data.Monoid                          ((<>))
import System.IO.Unsafe                     (unsafePerformIO)

import Paradox.Logger.Global                ( logVerboseG
                                            , logTraceG
                                            , Log(..)
                                            )

import qualified Data.Time.Clock.POSIX      as POSIX
import qualified Data.Text                  as TS

timeIt :: String -> IO a -> IO a
timeIt str action = do
    start <- POSIX.getPOSIXTime
    a <- action
    end <- POSIX.getPOSIXTime
    logVerboseG $ LogText $ TS.pack (show str) <> TS.pack (show $ end - start)
    return a

traceIt :: Show a => String -> a -> a
traceIt s c = unsafePerformIO $ do
    logTraceG $ LogText $ TS.pack s <> TS.pack (show c)
    return c

trace :: String -> a -> a
trace s c = unsafePerformIO $ do
    logTraceG $ LogText $ TS.pack s
    return c
