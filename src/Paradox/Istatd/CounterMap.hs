{-# LANGUAGE OverloadedStrings #-}
module Paradox.Istatd.CounterMap (
    CounterMap,
    lookupMagic,
    lookupC,
    lookupManyC,
    make,
    makeEmpty,
    convertForSearch,
    unpackSearch,
    deMaybe
) where

import Text.Regex.PCRE

import Paradox.Istatd.Types

import Paradox.Istatd.TextUtil      (convertGlobsToRegex, hadGlobs)

import qualified Data.Map.Strict    as M
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as TS


type CounterMap a = M.Map SearchCounterSpec a

data SearchCounterSpec = SearchCounterSpec {
      _scsName :: BS.ByteString
    , _scsOffset :: Maybe Int
    , _scsHasGlobs :: Bool
    } deriving (Eq, Ord, Show)


lookupManyC :: [CounterSpec] -> CounterMap a -> [(CounterSpec, a)]
lookupManyC cs m = concatMap (`lookupC` m) cs

lookupC :: CounterSpec -> CounterMap a -> [(CounterSpec, a)]
lookupC c m = unpackSearch . deMaybe . (`lookupMagic` m) . convertForSearch $ c

lookupMagic :: SearchCounterSpec -> CounterMap a -> [(SearchCounterSpec, Maybe a)]
lookupMagic n@(SearchCounterSpec _ _ hasGlobs) a = if hasGlobs
    then lookupGlob n a
    else [lookupSingle n a]

lookupSingle :: SearchCounterSpec -> CounterMap a -> (SearchCounterSpec, Maybe a)
lookupSingle n a = (n, M.lookup n a)

lookupGlob :: SearchCounterSpec -> CounterMap a -> [(SearchCounterSpec, Maybe a)]
lookupGlob n a = M.toList $ M.map Just $ filterGlob n a

filterGlob :: SearchCounterSpec -> CounterMap a -> CounterMap a
filterGlob (SearchCounterSpec s so _) = M.filterWithKey (\(SearchCounterSpec k o _) _ -> (k =~ s) && so == o)

make :: M.Map CounterSpec a -> CounterMap a
make = M.mapKeys convertForSearch


makeEmpty :: CounterMap a
makeEmpty = M.empty

--Takes a counterspec with normal or globbed searches and makes it into
--a search spec with regex searches
convertForSearch :: CounterSpec -> SearchCounterSpec
convertForSearch (CounterSpec k o) = SearchCounterSpec (convertGlobsToRegex k) o (hadGlobs k)

unpackSearch :: [(SearchCounterSpec, a)] -> [(CounterSpec, a)]
unpackSearch = map unpackSearch'

unpackSearch' :: (SearchCounterSpec, a) -> (CounterSpec, a)
unpackSearch' (SearchCounterSpec k o _, t) = (CounterSpec (TS.decodeUtf8 k) o, t)

deMaybe :: [(a, Maybe b)] -> [(a, b)]
deMaybe xs = [(a,b) | (a, Just b) <- xs]
