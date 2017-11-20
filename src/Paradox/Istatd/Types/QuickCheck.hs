{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}
module Paradox.Istatd.Types.QuickCheck where

import Paradox.Istatd.Types
import Paradox.Istatd.Functions
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Control.Monad
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Text as T
import Data.Map.Strict as M

arbitraryLegalTime :: Gen UTCTime
arbitraryLegalTime = do
    t <- (arbitrary :: Gen POSIXTime) `suchThat` (> 0)
    return $ posixSecondsToUTCTime t

maxTime :: Num a => a
maxTime = 5*365*24*60*60

inRange :: UTCTime -> UTCTime -> Bool
inRange s c = (diffUTCTime c s < fromIntegral maxTime)
              && c > s
              && diffUTCTime c s > 300

instance Arbitrary TimeRange where
    arbitrary = do
        s <- arbitraryLegalTime
        e <- arbitraryLegalTime `suchThat` inRange s
        return $ TimeRange (TimeWrapper s) (TimeWrapper e)

instance Arbitrary TimeRangeE where
    arbitrary = do
        s <- arbitraryLegalTime
        e <- arbitraryLegalTime `suchThat` inRange s
        return $ TimeRangeE (TimeWrapper s) (TimeWrapper e)

instance Arbitrary PostReply where
    arbitrary = sized arbitrarySizedPostReply

arbitraryInterval :: Gen Int
arbitraryInterval =
    choose(10, 86400)

arbitrarySizedPostReply :: Int -> Gen PostReply
arbitrarySizedPostReply n = do
        let inRange' tr' c = floor (fromIntegral (timeSpan tr') / fromIntegral c) >= 10
                            && floor (fromIntegral (timeSpan tr') / fromIntegral c) <= 4000
        tr <- arbitrary
        i <- arbitraryInterval `suchThat` inRange' tr

        ts <- arbitraryTSM n tr i
        return $ PostReply tr i ts


arbitraryTSM :: Int -> TimeRange -> Int -> Gen (Map Text TimeSeries)
arbitraryTSM n tr i = do
    v <- choose (1, n)
    tss <- replicateM v  $ do
        t <- arbitrary
        ts <- arbitraryTS tr i
        return (t,ts)
    return $ M.fromList tss

arbitraryTS :: TimeRange -> Int -> Gen TimeSeries
arbitraryTS tr i = do
    let s = floor $ utcTimeToPOSIXSeconds (unTimeWrapper $ trStart tr)
        e = floor $ utcTimeToPOSIXSeconds (unTimeWrapper $ trStop tr)
    tscs <- mapM arbitraryTSC [s,(s+fromIntegral i )..e]
    return $ TimeSeries (toEnd tr) i tscs

arbitraryTSC :: Integer -> Gen TimeSeriesChunk
arbitraryTSC t = do
    minV <- arbitrary
    maxV <- arbitrary `suchThat` (>= minV)
    countV <- arbitrary `suchThat` (> 0)
    avgV <- arbitrary `suchThat` (\c -> maxV >= c && c >= minV)
    let sumV = avgV * fromIntegral countV
        sumsq = sumV ** 2
    sdev <- arbitrary
    return $ TimeSeriesChunk t countV minV maxV sumV sumsq avgV sdev



--a <- (fmap . fmap) (Prelude.map (Prelude.length . tsData) . M.elems . prKeys) $ sample' (resize 1 $ arbitrary :: Gen PostReply)
