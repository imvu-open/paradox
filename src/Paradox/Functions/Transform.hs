{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Paradox.Functions.Transform (
  cons
, alias
, smartAlias
, smartAliasA
) where

import Data.Foldable                    ( foldl' )
import Data.Monoid                      ((<>))
import Language.Paradox.Eval.Types      ( WorldConstraints )
import Text.Regex.PCRE                  ((=~))
import Paradox.Istatd.Types             ( ParadoxTimeSeries(..)
                                        , CounterName
                                        , CounterSpec(..)
                                        , ReturnData
                                        , ParadoxReturn(..)
                                        )
import Safe                             ( atMay )

import qualified Data.ByteString        as BS
import qualified Data.Map.Strict        as M
import qualified Data.Text              as TS
import qualified Data.Text.Encoding     as TSE

(!!!)
  :: [a]
  -> Int
  -> Maybe a
(!!!) = atMay

cons
  :: WorldConstraints
  -> ReturnData
  -> ReturnData
  -> ReturnData
cons _w ParadoxReturn { timeSeriesMap = m1, options = o1, axisOptions = a1 } ParadoxReturn { timeSeriesMap = m2, options = o2, axisOptions = a2 } =
  ParadoxReturn
    { timeSeriesMap = M.union m1 m2
    , options = o1 <> o2
    , axisOptions = a1 <> a2
    }

alias
  :: WorldConstraints
  -> CounterName
  -> ReturnData
  -> ReturnData
alias _w newName pt@ParadoxReturn { timeSeriesMap = istatdData } =
  pt
    { timeSeriesMap = M.mapWithKey alias' $ M.mapKeys (const $ CounterSpec newName Nothing) istatdData }
  where
    alias' :: CounterSpec -> ParadoxTimeSeries -> ParadoxTimeSeries
    alias' cs n = n { ptsName = TS.pack . show $ cs }

regexAlias
  :: BS.ByteString
regexAlias = "(?:.*\\()?(?P<name>[-\\w*\\.]+)(?:,|\\)?.*)?"

smartAlias
  :: WorldConstraints
  -> CounterName
  -> Int
  -> ReturnData
  -> ReturnData
smartAlias _w namePattern node pt@ParadoxReturn { timeSeriesMap = istatdData } =
  pt
    { timeSeriesMap = M.mapWithKey alias' $ M.mapKeys nodeAlias istatdData }
  where
    alias' :: CounterSpec -> ParadoxTimeSeries -> ParadoxTimeSeries
    alias' cs n = n { ptsName = TS.pack . show $ cs }

    nodeAlias cs@CounterSpec {..} =
      let
        parts = TS.splitOn "." $ TSE.decodeUtf8 $ head (TSE.encodeUtf8 csName =~ regexAlias :: [[BS.ByteString]]) !! 1
        replacement = maybe csName (\part -> TS.replace "{}" part  namePattern) (parts !!! node)
      in
        cs { csName = replacement }

smartAliasA
  :: WorldConstraints
  -> CounterName
  -> [Int]
  -> ReturnData
  -> ReturnData
smartAliasA _w namePattern nodes pt@ParadoxReturn { timeSeriesMap = istatdData } =
  pt
    { timeSeriesMap = M.mapWithKey alias' $ M.mapKeys nodeAlias istatdData }
  where
    alias' :: CounterSpec -> ParadoxTimeSeries -> ParadoxTimeSeries
    alias' cs n = n { ptsName = TS.pack . show $ cs }

    nodeAlias cs@CounterSpec {..} =
      let
        parts = TS.splitOn "." $ TSE.decodeUtf8 $ head (TSE.encodeUtf8 csName =~ regexAlias :: [[BS.ByteString]]) !! 1
        replacement name node = maybe csName (\part -> TS.replace (TS.pack ("{" ++ show node ++ "}")) part name) (parts !!! node)
      in
        cs { csName = foldl' replacement namePattern nodes }
