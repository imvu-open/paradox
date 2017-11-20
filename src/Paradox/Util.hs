{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Paradox.Util where

import Data.Maybe                           ( fromMaybe )
import Data.Text                            (Text)
import Data.Aeson                           ( ToJSON(..)
                                            , Value (..)
                                            )

import Data.Aeson.Types                     ( Pair )
import Data.Foldable                        ( foldl' )
import qualified Data.HashMap.Strict        as H
import qualified Data.Map.Strict            as M

descriptiveHead :: String -> [a] -> a
descriptiveHead e l = case l of
    [] -> error e
    (x:_) -> x

safeHead :: [a] -> Maybe a
safeHead l = case l of
    [] -> Nothing
    (x:_) -> Just x

class SimplePartitionable a where
    spartition :: [a] -> ([a], [a])
    spartition = foldl' split ([], []) . map spivot
        where
            split (errs, goods) = \case
                Left a -> (a:errs, goods)
                Right a -> (errs, a:goods)

    spivot :: a -> Either a a

class Partitionable a b c where
    partition :: [a (Either b c)] -> ([a b], [a c])
    partition = foldl' split ([],[]) . map pivot
        where
        split(errs,goods) = \case
            Left a -> (a:errs, goods)
            Right a -> (errs, a:goods)

    pivot :: a (Either b c) -> Either (a b) (a c)

mapToAesonListFiltered :: ToJSON a
                       => M.Map Text a
                       -> [Text]
                       -> [Pair]
mapToAesonListFiltered values exc = mapToAesonList
                                  $ M.filterWithKey (\x _ -> notElem x exc)
                                  values

mapToAesonList :: ToJSON a
               => M.Map Text a
               -> [Pair]
mapToAesonList values = H.toList $ M.foldrWithKey (\k -> H.insert k . toJSON) H.empty
                                 values

toAesonList :: ToJSON a
            => a
            -> [Pair]
toAesonList v = case aesonV of
                  Object v' ->
                      H.toList v'
                  _ -> []
    where
        aesonV = toJSON v

-- | A variant of 'foldl' that has no base case,
-- and thus may only be applied to non-empty structures.
foldl1' :: Foldable t => (a -> a -> a) -> t a -> a
foldl1' f xs = fromMaybe (errorWithoutStackTrace "foldl1': empty structure")
                  (foldl' mf Nothing xs)
  where
    mf m y = Just (case m of
                      Nothing -> y
                      Just x  -> f x y)
