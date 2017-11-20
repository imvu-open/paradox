{-# LANGUAGE OverloadedStrings #-}
module Paradox.Istatd.TextUtil where

import qualified Data.Text as TS
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TSE

convertGlobsToRegex :: TS.Text -> BS.ByteString
convertGlobsToRegex = rejigger
    where
        rejigger r
            | hadGlobs r = TSE.encodeUtf8 . regex $ TS.snoc r '$'
            | otherwise = TSE.encodeUtf8 r

        regex = regexStars . regexQuestions . regexDots
        regexStars = TS.replace "*" "[^.]*?"
        regexQuestions = TS.replace "?" "."
        regexDots = TS.replace "." "[.]"

hadGlobs :: TS.Text -> Bool
hadGlobs = hasGlobs
    where
        hasGlobs x = any ($ x) [hasStars, hasQuestions]
        hasStars = TS.isInfixOf "*"
        hasQuestions = TS.isInfixOf "?"
