{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Paradox.Istatd.Types where

import GHC.Generics
import Data.Default
import Control.DeepSeq
import Data.Hashable
import Data.Typeable

import Paradox.Events.Types
import "lifted-base" Control.Exception.Lifted

import Data.Monoid                                  ((<>))

import Data.List.NonEmpty                           (NonEmpty(..))
import Data.Maybe                                   (catMaybes)
import Control.Applicative                          ((<|>))
import Data.Aeson                                   ( Value(..)
                                                    , (.:)
                                                    , (.=)
                                                    , (.:?)
                                                    , (.!=)
                                                    , withObject
                                                    , withText
                                                    , parseJSON
                                                    , toJSON
                                                    , FromJSON
                                                    , ToJSON
                                                    , ToJSONKey (..)
                                                    , object
                                                    , genericToJSON
                                                    , genericParseJSON
                                                    , defaultOptions
                                                    )
import Data.Aeson.Types                             ( emptyObject
                                                    , camelTo2
                                                    , toJSONKeyText
                                                    , Options (..)
                                                    )
import Data.Time.Clock                              ( UTCTime
                                                    , utctDay
                                                    , utctDayTime
                                                    )
import Data.Time.Calendar                           (toModifiedJulianDay)
import Data.Ratio                                   (numerator)
import Web.Scotty                                   ( Parsable
                                                    , parseParam
                                                    )

import Paradox.Util                                 ( Partitionable(..)
                                                    , toAesonList
                                                    )

import qualified Paradox.Istatd.CounterTree         as CT
import qualified Data.Time.Clock.POSIX              as POSIX
import qualified Data.Map.Strict                    as M
import qualified Data.HashMap.Strict                as H
import qualified Data.Text                          as TS
import qualified Data.Text.Encoding                 as TSE
import qualified Data.Text.Lazy.Encoding            as TLE
import qualified Language.Paradox.Grammar           as G
import qualified Data.Vector                        as V
import qualified Data.List.NonEmpty                 as NonEmpty
import qualified Data.ByteString.Lazy               as BSL


default                                             ( Double
                                                    , Integer
                                                    )

data Template
  = Template
  { tTemplate :: TS.Text
  , tName :: TS.Text
  , tReplacements :: H.HashMap TS.Text TS.Text
  } deriving (Show, Read, Eq, Generic)

instance ToJSON Template where
  toJSON = genericToJSON
         $ defaultOptions
         { fieldLabelModifier = camelTo2 '_' . drop 1
         , omitNothingFields = True
         }

instance FromJSON Template where
  parseJSON = genericParseJSON
            $ defaultOptions
            { fieldLabelModifier = camelTo2 '_' . drop 1
            , omitNothingFields = True
            }

-- | Typed draw option
data DrawOption
    -- | Value-less draw option
    = DrawOptionNV
    -- | Integer draw option
    | DrawOptionI Int
    -- | Double draw option
    | DrawOptionD Double
    -- | Text draw option
    | DrawOptionS TS.Text
    -- | Bool draw option
    | DrawOptionB Bool
    -- | Integer array draw option
    | DrawOptionIA [Int]
    -- | Double array draw option
    | DrawOptionDA [Double]
    -- | Text array draw option
    | DrawOptionSA [TS.Text]
    -- | Bool array draw option
    | DrawOptionBA [Bool]
    -- | Recursive Draw options
    | DrawOptionR DrawOptions
    deriving (Show, Read, Eq)

-- | Draw options
data DrawOptions where
    -- | Represents no options
    NoOptions :: DrawOptions
    -- | Represents raw json text as options
    RawOptions :: TS.Text  -> DrawOptions
    -- | A mapping of text onto 'DrawOption'
    MappedOptions :: M.Map TS.Text DrawOption -> DrawOptions

deriving instance Show DrawOptions
deriving instance Read DrawOptions
deriving instance Eq DrawOptions

instance Monoid DrawOption where
    mempty = DrawOptionR NoOptions

    mappend (DrawOptionR d) (DrawOptionR d') = DrawOptionR $ d <> d'
    mappend _ r = r

instance Monoid DrawOptions where
    mempty = NoOptions

    mappend (MappedOptions ml) (MappedOptions mr) = MappedOptions $ M.unionWith (<>) ml mr

    mappend NoOptions r = r
    mappend l NoOptions = l

    mappend (RawOptions l) _ = RawOptions l
    mappend _ (RawOptions r) = RawOptions r


-- | A paradox time series representation. Has a time range,
-- interval, name, draw options and a non empty list of 'TimeSeriesChunk'
data ParadoxTimeSeries = ParadoxTimeSeries
    { ptsRange :: TimeRangeE -- ^ TimeRangeE for this time series
    , ptsInterval :: {-# UNPACK #-} !Int -- ^ Interval for this time series
    , ptsData :: NonEmpty TimeSeriesChunk -- ^ NonEmpty list of TimeSeriesChunks
    , ptsName :: CounterName -- ^ Name of this time series
    , ptsDrawOptions :: DrawOptions -- ^ Draw options for this time series
    } deriving (Show, Eq)

instance Default ParadoxTimeSeries where
    def = ParadoxTimeSeries def 0 (def :| [] ) "" mempty

instance Default TimeRangeE where
    def = TimeRangeE (TimeWrapper $ POSIX.posixSecondsToUTCTime 0) (TimeWrapper $ POSIX.posixSecondsToUTCTime 0)

instance Default TimeRange where
    def = TimeRange (TimeWrapper $ POSIX.posixSecondsToUTCTime 0) (TimeWrapper $ POSIX.posixSecondsToUTCTime 0)

instance Default TimeSeriesChunk where
    def = TimeSeriesChunk 0 0 0.0 0.0 0.0 0.0 0.0 0.0

-- | A converted istatd time series. NonEmpty list instead of list
data TimeSeriesN = TimeSeriesN
    { tsnRange :: TimeRangeE
    , tsnInterval :: {-# UNPACK #-} !Int
    , tsnData :: NonEmpty TimeSeriesChunk
    } deriving (Show, Generic)

-- | An istatd time series
data TimeSeries = TimeSeries
    { tsRange :: TimeRangeE
    , tsInterval :: {-# UNPACK #-} !Int
    , tsData :: [TimeSeriesChunk]
    } deriving (Show, Generic)

-- | A single sample of istatd data.
data TimeSeriesChunk = TimeSeriesChunk
    { tscTime :: !Integer
    , tscCount :: {-# UNPACK #-} !Int
    , tscMin :: {-# UNPACK #-} !Double
    , tscMax :: {-# UNPACK #-} !Double
    , tscSum :: {-# UNPACK #-} !Double
    , tscSumsq :: {-# UNPACK #-} !Double
    , tscAvg :: {-# UNPACK #-} !Double
    , tscSdev :: {-# UNPACK #-} !Double
    } deriving (Show, Read, Eq, Generic, Typeable)

class ToTimeSeriesChunk a where
    toTimeSeriesChunk :: a -> TimeSeriesChunk

type LabeledTimeSeriesMap = M.Map CounterSpec ParadoxTimeSeries

data ParadoxReturn = ParadoxReturn
    { timeSeriesMap :: LabeledTimeSeriesMap
    , options :: DrawOptions
    , axisOptions :: DrawOptions
    } deriving(Show, Typeable)

type TSMap = LabeledTimeSeriesMap
type ReturnData = ParadoxReturn


instance Default ParadoxReturn where
    def = ParadoxReturn { timeSeriesMap = mempty, options = mempty, axisOptions = mempty }

instance Show CounterSpec where
    show CounterSpec {..} = concat $ catMaybes [Just (TS.unpack csName), fmap ((++) "##" . show) csOffset]

data PostReplyErrorBody = PostReplyErrorBody String BSL.ByteString String
    deriving (Show, Eq, Typeable)

instance Exception PostReplyErrorBody

instance ToJSON PostReplyErrorBody where
    toJSON (PostReplyErrorBody err resp msg) = object [
              "error" .= err
            , "response_body" .= TLE.decodeUtf8 resp
            , "message" .= msg
        ]

data PostReplyRet
    = PostReplySuccess (Maybe Int) PostReply
    | PostReplyError PostReplyErrorBody
    deriving (Show)

data PostReply = PostReply
    { prRange       :: TimeRange
    , prInterval  :: {-# UNPACK #-} !Int
    , prKeys        :: M.Map CounterName TimeSeries
    } deriving (Show, Generic)

data PostReplyN = PostReplyN
    { prnRange       :: TimeRange
    , prnInterval  :: {-# UNPACK #-} !Int
    , prnKeys        :: M.Map CounterName TimeSeriesN
    } deriving (Show, Generic)


data ParadoxReply = ParadoxReply
    { pprRange       :: TimeRange
    , pprInterval  :: {-# UNPACK #-} !Int
    , pprQueries        :: M.Map TS.Text ParadoxReturn
    , pprEvents :: M.Map TS.Text [EventData]
    } deriving (Show, Generic)


data EventReq = EventReq
    { erName :: TS.Text
    , erIdent :: EventFilter
    , erKeys :: [TS.Text]
    } deriving (Show)

instance FromJSON EventReq where
    parseJSON = withObject "FromJSON EventReq" $ \o ->
        EventReq <$> o .: "name"
                 <*> o .: "ident"
                 <*> o .: "keys"

--Paradox POST query
data ParadoxQuery = ParadoxQuery
    { pqRange       :: TimeRange
    , pqKeys        :: [ContextExpr (Either G.ParadoxParseError G.Expr)]
    , pqEvents      :: [EventReq]
    , pqMaxSamples  :: {-# UNPACK #-} !Int
    } deriving (Show)

--Paradox POST query
data ParadoxRenderQuery = ParadoxRenderQuery
    { prqRange :: TimeRange
    , prqMaxSamples :: {-# UNPACK #-} !Int
    , prqWidth :: {-# UNPACK #-} !Double
    , prqHeight :: {-# UNPACK #-} !Double
    , prqEmailTo :: Maybe TS.Text
    , prqInlineHTML :: Bool
    , prqGraphs :: M.Map TS.Text GraphData
    } deriving (Show)

data GraphData = GraphData
    { gdKeys :: [ContextExpr (Either G.ParadoxParseError G.Expr)]
    , gdDrawOptions :: M.Map TS.Text TS.Text
    } deriving (Show)

data ContextExpr a = CExpr
    { ceQS :: TS.Text
    , ceQ  :: a
    } deriving (Show)

instance ToJSON a => ToJSON (ContextExpr a) where
    toJSON CExpr {..} = object [
              "queryString" .= ceQS
            , "data" .= ceQ
        ]


instance Partitionable ContextExpr b c where
    pivot = \case
        CExpr t (Left l) -> Left $ CExpr t l
        CExpr t (Right r) -> Right $ CExpr t r

textToCExpr :: TS.Text
            -> ContextExpr (Either G.ParadoxParseError G.Expr)
textToCExpr o = CExpr o (p' $ G.parseExpr $ TSE.encodeUtf8 o)
    where
        p' (Left e) = Left e
        p' (Right r) = Right r

instance FromJSON (ContextExpr (Either G.ParadoxParseError G.Expr)) where
    parseJSON = withText "FromJSON  ContextExpr" $ \o ->
        return $ textToCExpr o

instance FromJSON ParadoxQuery where
    parseJSON = withObject "FromJSON ParadoxQuery" $ \o ->
        ParadoxQuery <$> parseJSON (Object o)
                <*> o .: "keys"
                <*> o .: "events"
                <*> o .: "maxSamples"

instance Parsable a => Parsable (Maybe a) where
    parseParam p = case parseParam p of
                   Right x -> Right . Just $ x
                   Left _ -> Left "Uh"


instance FromJSON ParadoxRenderQuery where
    parseJSON = withObject "FromJSON ParadoxRenderQuery" $ \o ->
        ParadoxRenderQuery <$> parseJSON (Object o)
                <*> o .: "maxSamples"
                <*> o .:? "width" .!= 800.0
                <*> o .:? "height" .!= 600.0
                <*> o .:? "email"
                <*> o .:? "inline_html" .!= False
                <*> o .: "graphs"

instance FromJSON GraphData where
    parseJSON = withObject "FromJSON GraphData" $ \o ->
        GraphData <$> o .: "keys"
                <*> o .: "drawOptions"

data PostRequestCompact = PostRequestCompact
    { preqcRange       :: TimeRange
    , preqcKeys        :: [CounterName]
    , preqcMaxSamples  :: {-# UNPACK #-} !Int
    , preqcCompact      :: !Bool
    }

--ISTATD POST request
data PostRequest = PostRequest
    { preqRange       :: TimeRange
    , preqKeys        :: [CounterName]
    , preqMaxSamples  :: {-# UNPACK #-} !Int
    } deriving (Show)

instance ToJSON PostRequest where
    toJSON (PostRequest preqRange preqKeys' preqMaxSamples') = object $ [
          "keys" .= preqKeys'
        , "maxSamples" .= preqMaxSamples'
        ] ++ range
        where
            range = toAesonList preqRange

instance ToJSON PostRequestCompact where
    toJSON (PostRequestCompact preqcRange preqcKeys' preqcMaxSamples' preqcCompact') = object $ [
          "keys" .= preqcKeys'
        , "maxSamples" .= preqcMaxSamples'
        , "compact" .= preqcCompact'
        ] ++ range
        where
            range = toAesonList preqcRange

instance FromJSON PostRequest where
    parseJSON = withObject "FromJSON PostRequest" $ \o ->
        PostRequest <$> parseJSON (Object o)
                <*> o .: "keys"
                <*> o .: "maxSamples"

--ISTATD Counter Reply
--TODO: Newtype?
data CountersTypeMapping = CountersTypeMapping
    {
      ctmMapping :: M.Map TS.Text TS.Text
    } deriving (Show, Generic)

data CountersMatch = CountersMatch
    { cmIsLeaf :: !Bool
    , cmType :: {-# UNPACK #-} !Int
    , cmName :: CounterName
    } deriving (Read, Show, Generic)

data CountersReply = CountersReply
    { crTypeMapping :: CountersTypeMapping
    , crPattern :: String
    , crMatch :: [CountersMatch]
    } deriving (Show, Generic)

data ParadoxCountersReply = ParadoxCountersReply
    { pcrTypeMapping :: CountersTypeMapping
    , pcrPattern :: TS.Text
    , pcrMatch :: CT.SuperHashMap
    } deriving (Show, Generic)


--request types
data TimeRange = TimeRange
    { trStart :: {-# UNPACK #-} !TimeWrapper
    , trStop :: {-# UNPACK #-} !TimeWrapper
    } deriving(Show, Read, Eq, Generic)

toPicoSeconds :: UTCTime
              -> Integer
toPicoSeconds t = numerator x
  where
    x     = toRational day * 86400 * pico + psecs * pico
    day   = toModifiedJulianDay (utctDay t)
    psecs = toRational (utctDayTime t)
    pico  = 10^(12 :: Integer)

newtype TimeWrapper = TimeWrapper { unTimeWrapper :: UTCTime } deriving (Show, Read, Eq, Generic)

instance Hashable TimeRange
instance Hashable TimeWrapper where
    hash t = hash (toPicoSeconds $ unTimeWrapper t)
    hashWithSalt x t = hashWithSalt x (toPicoSeconds $ unTimeWrapper t)

--SO FRUSTRATING. IStatd randomly swaps between end and stop
data TimeRangeE = TimeRangeE
    { treStart :: {-# UNPACK #-} !TimeWrapper
    , treEnd :: {-# UNPACK #-} !TimeWrapper
    } deriving(Show, Read, Eq, Generic)

type CounterName = TS.Text

data ErrorWrapper = forall a. (Show a, ToJSON a) => MkErrorWrapper TS.Text a

instance ToJSON ErrorWrapper where
    toJSON (MkErrorWrapper s a) = object [
              "message" .= s
            , "inner" .= a
        ]

instance Show ErrorWrapper where
    show (MkErrorWrapper s a) =  "ErrorWrapper message " ++ show s ++ " inner " ++ show a

data CounterNameParts
    = CNPString TS.Text
    | CNPGlobStar
    | CNPGlobQuestion
    | CNPDot
    | CNPShellReplacement TS.Text
    | CNPShellReplaced TS.Text
    deriving (Eq, Ord, Generic, Show)

type CounterNameInParts = [CounterNameParts]

data SafeCounterNameParts
    = SCNPString TS.Text
    | SCNPGlobStar
    | SCNPGlobQuestion
    | SCNPDot
    | SCNPShellReplaced TS.Text
    deriving (Eq, Ord, Generic, Show)

type SafeCounterNameInParts = [SafeCounterNameParts]

data CounterSpecIntermediate = CounterSpecIntermediate
    { csiName :: [CounterNameParts]
    , csiOffset :: Maybe Int
    } deriving (Eq, Ord, Generic, Show)

data CounterSpec = CounterSpec
    { csName :: CounterName
    , csOffset :: Maybe Int
    } deriving (Eq, Ord, Generic)

instance Hashable CounterSpec

floorI :: (RealFrac a)
       => a
       -> Integer
floorI = floor

fromIntegralI :: (Num b)
              => Integer
              -> b
fromIntegralI = fromIntegral

instance ToJSON TimeRange where
    toJSON TimeRange {..} = object [
          "start" .= floorI (POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper trStart)
        , "stop" .= floorI (POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper trStop)
        ]

instance FromJSON TimeRange where
    parseJSON = withObject "FromJSON TimeRange" $ \o ->
        TimeRange <$> fmap (TimeWrapper . POSIX.posixSecondsToUTCTime . fromIntegralI) (o .: "start")
                  <*> fmap (TimeWrapper . POSIX.posixSecondsToUTCTime . fromIntegralI) (o .: "stop" <|> o .: "end")

instance ToJSON TimeRangeE where
    toJSON TimeRangeE {..} = object [
          "start" .= floorI (POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper treStart)
        , "end" .= floorI (POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper treEnd)
        ]

instance FromJSON TimeRangeE where
    parseJSON = withObject "FromJSON TimeRangeE" $ \o ->
        TimeRangeE <$> fmap (TimeWrapper . POSIX.posixSecondsToUTCTime . fromIntegralI) (o .: "start")
                  <*> fmap (TimeWrapper . POSIX.posixSecondsToUTCTime . fromIntegralI) (o .: "stop" <|> o .: "end")

instance FromJSON CountersTypeMapping where
    parseJSON = withObject "FromJSON CountersTypeMapping" $ \o ->
        CountersTypeMapping <$> ( parseJSON
                                . Object
                                ) o

instance ToJSON CountersTypeMapping where
    toJSON CountersTypeMapping {..} = object pairs
        where
            pairs = map (uncurry (.=)) $ M.toList ctmMapping

instance FromJSON CountersMatch where
    parseJSON = withObject "FromJSON CountersMatch" $ \o ->
        CountersMatch <$> o .: "is_leaf"
                <*> o .: "type"
                <*> o .: "name"

instance ToJSON CountersMatch where
    toJSON CountersMatch {..} = object [
          "is_leaf" .= cmIsLeaf
        , "type" .= cmType
        , "name" .= cmName
        ]

instance FromJSON CountersReply where
    parseJSON = withObject "FromJSON CountersReply" $ \o ->
        CountersReply <$> o .: "type_mapping"
                <*> o .: "pattern"
                <*> o .: "matching_names"

instance ToJSON CountersReply where
    toJSON CountersReply {..} = object [
          "type_mapping" .= crTypeMapping
        , "pattern" .= crPattern
        , "matching_names" .= crMatch
        ]

instance ToJSON ParadoxCountersReply where
    toJSON ParadoxCountersReply {..} = object [
          "type_mapping" .= pcrTypeMapping
        , "pattern" .= pcrPattern
        , "matching_names" .= pcrMatch
        ]

instance ToJSON ParadoxTimeSeries where
    toJSON ParadoxTimeSeries {..} = object $ [
          "interval" .= ptsInterval
        , "data" .= NonEmpty.toList ptsData
        , "name" .= ptsName
        , "graph_opts" .= ptsDrawOptions
        ] ++ range
        where
            range = toAesonList ptsRange

instance ToJSON DrawOptions where
    toJSON NoOptions = object []
    toJSON (RawOptions x) = toJSON x
    toJSON (MappedOptions v) = toJSON v

instance ToJSON DrawOption where
   toJSON DrawOptionNV = toJSON emptyObject
   toJSON (DrawOptionI v) = toJSON v
   toJSON (DrawOptionD v) = toJSON v
   toJSON (DrawOptionS v) = toJSON v
   toJSON (DrawOptionB v) = toJSON v
   toJSON (DrawOptionIA v) = toJSON v
   toJSON (DrawOptionDA v) = toJSON v
   toJSON (DrawOptionSA v) = toJSON v
   toJSON (DrawOptionBA v) = toJSON v
   toJSON (DrawOptionR v) = toJSON v


instance FromJSON TimeSeries where
    parseJSON = withObject "FromJSON TimeSeries" $ \o ->
        TimeSeries <$> parseJSON (Object o)
            <*> o .: "interval"
            <*> tsdata o
        where
            tsdata o = (fmap extractF <$> (o .: "data")) <|> (fmap extractC <$> (o .: "data"))
            extractF (TimeSeriesChunkFat t) = t
            extractC (TimeSeriesChunkCompact t) = t

instance ToJSON TimeSeries where
    toJSON TimeSeries {..} = object $ [
          "interval" .= tsInterval
        , "data" .= tsData
        ] ++ range
        where
            range = toAesonList tsRange

newtype TimeSeriesChunkFat = TimeSeriesChunkFat TimeSeriesChunk
newtype TimeSeriesChunkCompact = TimeSeriesChunkCompact TimeSeriesChunk


instance FromJSON TimeSeriesChunkCompact where
    parseJSON a = do
        [time, count, minV, maxV, sumV ,sumsq, avg, sdev] <- parseJSON a
        return $ TimeSeriesChunkCompact (TimeSeriesChunk
            (floor time)
            (floor count)
            minV
            maxV
            sumV
            sumsq
            avg
            sdev)

instance FromJSON TimeSeriesChunkFat where
    parseJSON = withObject "FromJSON TimeSeriesChunkFat" $ \o ->
        TimeSeriesChunkFat <$> (TimeSeriesChunk <$> o .: "time"
            <*> o .: "count"
            <*> o .: "min"
            <*> o .: "max"
            <*> o .: "sum"
            <*> o .: "sumsq"
            <*> o .: "avg"
            <*> o .: "sdev")

instance ToJSON TimeSeriesChunk where
    toJSON TimeSeriesChunk {..} = object [
          "time" .= tscTime
        , "count" .= tscCount
        , "min" .= tscMin
        , "max" .= tscMax
        , "sum" .= tscSum
        , "sumsq" .= tscSumsq
        , "avg" .= tscAvg
        , "sdev" .= tscSdev
        ]


instance ToJSON TimeSeriesChunkCompact where
    toJSON (TimeSeriesChunkCompact TimeSeriesChunk {..}) = Array . V.fromList $
        [toJSON tscTime, toJSON tscCount, toJSON tscMin, toJSON tscMax, toJSON tscSum, toJSON tscSumsq, toJSON tscAvg, toJSON tscSdev]

instance FromJSON PostReply where
    parseJSON = withObject "FromJSON PostReply" $ \o ->
        PostReply <$> parseJSON (Object o)
            <*> o .: "interval"
            <*> ( parseJSON
                . Object
                . H.filterWithKey (\k _ -> notElem k ["start", "stop", "interval"])
                ) o

instance ToJSON PostReply where
    toJSON PostReply {..} = object $ [
        "interval" .= prInterval
        ] ++ pairs ++ range
        where
            pairs = map (uncurry (.=)) $ M.toList prKeys
            range = toAesonList prRange


instance ToJSON CounterSpec where
    toJSON = String . TS.pack . show

instance ToJSONKey CounterSpec where
  toJSONKey = toJSONKeyText (TS.pack . show)

instance ToJSON ParadoxReply where
    toJSON ParadoxReply {..} = object $ [
          "interval" .= pprInterval
        , "results" .= pprQueries
        , "events" .= pprEvents] ++ range
        where
            range = toAesonList pprRange

instance ToJSON ParadoxReturn where
    toJSON ParadoxReturn {..} = object [
            "counters" .= timeSeriesMap
          , "options" .= options
          , "axis_options" .= axisOptions
        ]

instance NFData PostReply
instance NFData TimeSeries
instance NFData TimeSeriesChunk
instance NFData TimeRange
instance NFData TimeRangeE
instance NFData TimeWrapper

