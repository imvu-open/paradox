{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Paradox.Eval.Types where


import Language.Paradox.Eval.Types

import Data.Aeson                               ( ToJSON(..)
                                                , (.=)
                                                , object
                                                )
import Language.Paradox.Grammar                 ( ParadoxParseError )

import Paradox.Util                             ( Partitionable (..))

import qualified Data.List.Split                as LS
import qualified Data.Text                      as TS
import qualified Data.Text.Encoding             as TSE
import qualified Paradox.Istatd.Types           as IT
import qualified Data.ByteString.Lazy.Char8     as BSL
import qualified Paradox.Types                  as PT

data ContextEvaluable a = CEvaluable {
      cevQS :: TS.Text
    , cevQ  :: a
    }

instance Show a => Show (ContextEvaluable a) where
    show CEvaluable {..} = "Query: " ++ show cevQS ++ "\nResult: " ++ show cevQ

instance ToJSON a => ToJSON (ContextEvaluable a) where
    toJSON CEvaluable {..} = object [
              "queryString" .= cevQS
            , "data" .= cevQ
        ]

data QueryStatus =
      QueryMalformedSyntax BSL.ByteString [IT.ContextExpr ParadoxParseError]
    | QueryMisTypedQueries BSL.ByteString [ContextEvaluable ExprError]
    | QueryUnresolveableSandboxActions BSL.ByteString [ContextEvaluable IT.ErrorWrapper]
    | QuerySuccess [ContextEvaluable (Expr IT.ParadoxReturn)]
    | QueryUnresolvedSuccess [ContextEvaluable (Unresolved (Expr IT.ParadoxReturn))]
    deriving (Show)

instance ToJSON QueryStatus where
    toJSON (QueryMisTypedQueries e l) = object [
          "status" .= ("error" :: TS.Text)
        , "message" .= object [
             "message" .= TSE.decodeUtf8 (BSL.toStrict e)
           ,  "errors" .= l
           ]
        ]
    toJSON (QueryMalformedSyntax e l) = object [
          "status" .= ("error" :: TS.Text)
        , "message" .= object [
             "message" .= TSE.decodeUtf8 (BSL.toStrict e)
           ,  "errors" .= map (\IT.CExpr {..} -> IT.CExpr ceQS (fix $ show ceQ)) l
           ]
        ]
            where
                fix = LS.splitOn "\n"
    toJSON (QueryUnresolveableSandboxActions e l) = object [
          "status" .= ("error" :: TS.Text)
        , "message" .= object [
             "message" .= TSE.decodeUtf8 (BSL.toStrict e)
           ,  "errors" .= l
           ]
        ]
    toJSON _ = object []

data EvalResult = EvalResult
    { accumLogs :: Accum
    , numQueries :: !Int
    , exprPartsPerQuery :: !Double
    , funcPerQuery :: !Double
    , bindsPerQuery :: !Double
    , litsPerQuery :: !Double
    , passesPerQuery :: !Double
    , higherOrderPerQuery :: !Double
    }

data KeysStatus =
      KeysBackendError [IT.PostReplyErrorBody]
    | KeysSuccess [(Maybe Int, IT.PostReplyN)]

data EvalStatus =
      EvalSuccess IT.ParadoxReply EvalResult
    | EvalFailed (PT.RActionM ())

data QueryBounds = QueryBounds {
          qbTimeRange :: IT.TimeRange
        , qbMaxSamples :: Int
        , qbOrigTimeRange :: IT.TimeRange
        , qbOrigMaxSamples :: Int
    }

instance Partitionable ContextEvaluable b c where
    pivot = \case
        CEvaluable q (Left l) -> Left $ CEvaluable q l
        CEvaluable q (Right r) -> Right $ CEvaluable q r
