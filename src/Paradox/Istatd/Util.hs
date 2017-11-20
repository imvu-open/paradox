{-# LANGUAGE OverloadedStrings #-}
module Paradox.Istatd.Util where

import Paradox.Istatd.CounterMap
import Paradox.Istatd.Types

import Data.Maybe                               (fromJust)

import qualified Data.Text                      as TS
import qualified Data.Map.Strict                as M
import qualified Paradox.Istatd.CounterMap      as C
import qualified Paradox.Istatd.CounterTree     as CT

countersReplyToCounterMap :: CountersReply -> CounterMap CountersMatch
countersReplyToCounterMap c = finalCtrs
    where
        ctrsMatch = crMatch c
        ctrsPruned = filter cmIsLeaf ctrsMatch
        finalCtrs = C.make $ M.fromList $ map (\x -> (CounterSpec (cmName x) Nothing, x)) ctrsPruned


countersReplyToCounterTree :: CountersReply -> CT.SuperHashMap
countersReplyToCounterTree c = finalCtrs
    where
        ctrsMatch = crMatch c
        finalCtrs =
            let ctType x = fromJust $ CT.getCounterType $ cmType x
            in  CT.construct $ map (\x -> (TS.splitOn "." (cmName x), CT.IValue (cmIsLeaf x) (ctType x) ))
                                 $ filter cmIsLeaf ctrsMatch
