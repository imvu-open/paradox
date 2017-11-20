{-# LANGUAGE OverloadedStrings #-}
module Paradox.Functions.Generate (
  constantLine
) where

import Data.Default


import Language.Paradox.Eval.Types          (WorldConstraints(..))
import Paradox.Istatd.Types                 ( TimeSeriesChunk(..)
                                            , CounterSpec(..)
                                            , ParadoxTimeSeries(..)
                                            , TimeRange(..)
                                            , ReturnData
                                            , ParadoxReturn(..)
                                            , TimeWrapper(..)
                                            )
import Paradox.Istatd.Functions             (toEnd)

import qualified Paradox.Istatd.Lifted      as IstatdLifted
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as TS
import qualified Data.Time.Clock.POSIX      as POSIX
import qualified Data.List.NonEmpty         as NonEmpty

constantLine :: WorldConstraints
             -> Double
             -> ReturnData
constantLine w v =
    let tr = wTimeRange w
        s = floor $ POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper $ trStart tr
        e = floor $ POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper $ trStop tr
        tI = case wInterval w of
            Nothing -> maximum [10, (e - s) `div` wMaxSamples w]
            Just i -> maximum [10, i]
        ttr = IstatdLifted.normalizeRange tr tI
        ts = floor $ POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper $ trStart ttr
        te = floor $ POSIX.utcTimeToPOSIXSeconds $ unTimeWrapper $ trStop ttr
        confs = map (mkVal v) [ts,(ts+tI)..te]
        mkVal v' t = def {
                  tscAvg = v'
                , tscMin = v'
                , tscMax = v'
                , tscSum = v'
                , tscCount = 1
                , tscSumsq = v'**2
                , tscSdev = 0
                , tscTime = fromIntegral t
            }
        name = TS.concat ["constantLine(", TS.pack $ show v, ")"]
        val = do
            confs' <- NonEmpty.nonEmpty confs
            return $ def { ptsName = name
                         , ptsInterval = tI
                         , ptsData = confs'
                         , ptsRange = toEnd ttr
                         }
    in case val of
         Just val' -> ParadoxReturn { timeSeriesMap = M.singleton (CounterSpec name Nothing) val', options = mempty, axisOptions = mempty }
         Nothing -> def
