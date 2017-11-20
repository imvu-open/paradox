{-# LANGUAGE OverloadedStrings #-}
module Paradox.Functions.Util (
  Direction (..)
, Summarizer (..)
, directionComparison
, toInfix
, toPostfix
, tails
) where

import qualified Data.Text      as TS
import qualified Data.List      as L (tails)

data Direction = Above | Below | AboveInc | BelowInc

data Summarizer = SumSummarizer | AvgSummarizer
                deriving (Show)

directionComparison :: Ord a
                    => Direction
                    -> a
                    -> a
                    -> Bool
directionComparison d l r = case d of
    Above -> l < r
    Below -> l > r
    AboveInc -> l <= r
    BelowInc -> l >= r

toInfix :: Direction
        -> TS.Text
toInfix d = case d of
    Above -> "Above"
    Below -> "Below"
    AboveInc -> "Above"
    BelowInc -> "Below"

toPostfix :: Direction
          -> TS.Text
toPostfix d = case d of
    AboveInc -> "Inc"
    BelowInc -> "Inc"
    _ -> ""

-- Requires optimization
tails :: Int
      -> [a]
      -> [[a]]
tails lag' ts = filter (\x -> length x >= lag') $ map (take lag') $ L.tails ts
