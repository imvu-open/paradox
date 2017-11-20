{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Language.Paradox.Eval.TypeRep.Help (
  lookupByType
, findInAllSymbols
, findHelp
) where

import Data.Maybe                               ( catMaybes )
import Data.Aeson                               ( (.=)
                                                , toJSON
                                                , ToJSON
                                                , object
                                                )
import Language.Paradox.Eval.Types              ( Typ (..)
                                                , TypE (..)
                                                , (:==:)(..)
                                                , TypeMatch (..)
                                                , MatchCollection (..)
                                                , equalT
                                                )
import Language.Paradox.Eval.Symbols            (allSymbols)
import Language.Paradox.Eval.Help               ( helpToType )
import Language.Paradox.Eval.TypeRep.Parse      ( textToTyp )
import qualified Data.Map.Strict                as M
import qualified Data.Text                      as TS
import qualified Data.Char                      as C
import qualified Data.List.NonEmpty             as NonEmpty

lookupByType :: Typ a
             -> Typ b
             -> [MatchCollection]
             -> TypeMatch
lookupByType l q ps = case q of
    (left :~> right) -> case equalT right l of
        Just Refl -> PartialMatch $ toTyp $ NonEmpty.fromList $ MatchCollection left : ps
        Nothing -> lookupByType l right $ MatchCollection left : ps
    right -> case equalT right l of
        Just Refl -> case ps of
                       [] -> AllMatch
                       _ -> FullMatch $ toTyp $ NonEmpty.fromList ps
        Nothing -> NoneMatch

toTyp :: NonEmpty.NonEmpty MatchCollection
      -> MatchCollection
toTyp coll = case NonEmpty.length coll of
               1 -> NonEmpty.head coll
               _ -> foldr1 (\(MatchCollection e) (MatchCollection c) ->  MatchCollection $ e :~> c) coll

findInAllSymbols :: Typ a
                 -> M.Map TS.Text (TypE, TypeMatch)
findInAllSymbols l = M.filter (\(_,v) -> case v of
                                           NoneMatch -> False
                                           _ -> True
                              )
                   $ M.map (\t@(TypE s _) -> (t, lookupByType l s [])) allSymbols

findInAllSymbolsT :: TS.Text
                 -> M.Map TS.Text (TypE, TypeMatch)
findInAllSymbolsT t = case textToTyp t of
    MatchCollection m -> findInAllSymbols m

findHelp :: TS.Text
         -> [FunctionHelp]
findHelp t = catMaybes $ M.elems $ M.mapWithKey toHelp' $ findInAllSymbolsT t

toHelp' :: TS.Text
       -> (TypE, TypeMatch)
       -> Maybe FunctionHelp
toHelp' k (t,m) = do
    (f,r) <- TS.uncons $ TS.drop 6 k
    let name = TS.cons (C.toLower f) r
        mkFunHelp = pure . FunctionHelp name t m
    case m of
      AllMatch -> mkFunHelp Nothing
      NoneMatch -> Nothing
      PartialMatch a -> mkFunHelp (Just a)
      FullMatch a -> mkFunHelp (Just a)

data FunctionHelp = FunctionHelp TS.Text TypE TypeMatch (Maybe MatchCollection)

instance Show FunctionHelp where
    show (FunctionHelp t ty m mc) = "FunctionHelp " ++ show t ++ " " ++ show ty ++ " " ++ show m ++ " " ++ show mc

instance ToJSON FunctionHelp where
    toJSON (FunctionHelp t ty _ mc) = case ty of
        TypE typ _ -> object [
              "name" .= t
            , "type" .= helpToType typ
            , "argsNeeded" .= case mc of
                    Nothing -> toJSON ("none" :: TS.Text)
                    Just (MatchCollection mi) -> toJSON $ helpToType mi
            ]
