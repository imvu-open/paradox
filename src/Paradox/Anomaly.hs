{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Paradox.Anomaly where

import Prelude hiding (min, max)
import qualified GHC.Exts as Exts

import Paradox.Istatd.Types
import Paradox.Functions.Math.TimeSeriesChunk

import Data.DTW
import qualified Data.Foldable as Fold
import Data.Default

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.MultiSet as MS
import qualified Data.Vector as V
import qualified Data.List as L

import qualified Data.List


instance DataSet (NonEmpty.NonEmpty a) where
    type Item (NonEmpty.NonEmpty a) = a
    ix  = (NonEmpty.!!)
    len = NonEmpty.length


newtype LessTSC = LessTSC { unLessTSC :: TimeSeriesChunk } deriving (Eq, Show)
newtype GreaterTSC = GreaterTSC { unGreaterTSC :: TimeSeriesChunk } deriving (Eq, Show)

instance Ord LessTSC where
    LessTSC l `compare` LessTSC r = tscAvg l `compare` tscAvg r

instance Ord GreaterTSC where
    GreaterTSC l `compare` GreaterTSC r = tscAvg r `compare` tscAvg l

invokeDTW :: NonEmpty.NonEmpty TimeSeriesChunk
          -> NonEmpty.NonEmpty TimeSeriesChunk
          -> Result Double
invokeDTW = fastDtw dist shrink 2
    where
        dist :: TimeSeriesChunk
             -> TimeSeriesChunk
             -> Double
        dist a b = abs $ tscAvg $ subTimeSeriesChunk b a



        shrink :: NonEmpty.NonEmpty TimeSeriesChunk
               -> NonEmpty.NonEmpty TimeSeriesChunk
        shrink xs = case xs of
            (f NonEmpty.:| []) ->  NonEmpty.fromList [f]
            (f NonEmpty.:| (a:bs)) -> NonEmpty.fromList (divTimeSeriesChunkScalar 2 (addTimeSeriesChunk f a) : shrink' bs)
                where
                    shrink' xs' = case xs' of
                        (a':b:cs) -> divTimeSeriesChunkScalar 2 (addTimeSeriesChunk a' b) : shrink' cs
                        x -> x

stlSeason :: NonEmpty.NonEmpty TimeSeriesChunk
          -> Int
          -> NonEmpty.NonEmpty TimeSeriesChunk
stlSeason xandy interval =
    let (seasonal, _trend, _residual) = stl xandy interval
    in seasonal

stlTrend :: NonEmpty.NonEmpty TimeSeriesChunk
         -> Int
         -> NonEmpty.NonEmpty TimeSeriesChunk
stlTrend xandy interval =
    let (_seasonal, trend, _residual) = stl xandy interval
    in trend

stlResidual :: NonEmpty.NonEmpty TimeSeriesChunk
            -> Int
            -> NonEmpty.NonEmpty TimeSeriesChunk
stlResidual xandy interval =
    let (_seasonal, _trend, residual) = stl xandy interval
    in residual

splitByDay :: NonEmpty.NonEmpty TimeSeriesChunk
           -> [NonEmpty.NonEmpty TimeSeriesChunk]
splitByDay iNETSC =
    let day = 86400
        iLTSC = NonEmpty.toList iNETSC
        rank :: Integer
             -> Integer
             -> TimeSeriesChunk
             -> Integer
        rank period initial tsc = (tscTime tsc - initial) `mod` period
        oLTSC = Exts.groupWith (rank day (tscTime $ head iLTSC)) iLTSC
        in map NonEmpty.fromList oLTSC

stitch :: [NonEmpty.NonEmpty TimeSeriesChunk]
       -> NonEmpty.NonEmpty TimeSeriesChunk
stitch iLNETSC =
    let listOfLists = map NonEmpty.toList iLNETSC
        list = concat listOfLists
        sortedList = Exts.sortWith tscTime list
    in NonEmpty.fromList sortedList

stl :: NonEmpty.NonEmpty TimeSeriesChunk
    -> Int
    -> (NonEmpty.NonEmpty TimeSeriesChunk
      , NonEmpty.NonEmpty TimeSeriesChunk
      , NonEmpty.NonEmpty TimeSeriesChunk)
stl iNETSC interval =
    let
        initialTrend = fmap (mulTimeSeriesChunkScalar 0.0) iNETSC
        initialRobustnessWeights = repeat 0.0
        myDefaultNETSC = NonEmpty.fromList [def]
        closedOuter = stlOuterLoop iNETSC interval
        (seasonal, trend, residuals, _robustnessWeights) = iterate closedOuter (myDefaultNETSC, initialTrend, myDefaultNETSC, initialRobustnessWeights) !! 5
    in (seasonal, trend, residuals)

slopyMedian :: [Double]
            -> Double
slopyMedian x =
    let n = length x
        x' = Data.List.sort x
    in head $ drop (n `div` 2) x'

calculateRobustnessWeights :: NonEmpty.NonEmpty TimeSeriesChunk
                           -> [Double]
calculateRobustnessWeights residuals =
    let absoluteResiduals = map (abs . tscAvg) $ NonEmpty.toList residuals
        h = slopyMedian absoluteResiduals * 6.0
        calculateWeight absResidual = bisquare $ absResidual / h
        robustnessWeights = map calculateWeight absoluteResiduals
    in robustnessWeights

stlOuterLoop :: NonEmpty.NonEmpty TimeSeriesChunk
             -> Int
             -> (NonEmpty.NonEmpty TimeSeriesChunk
               , NonEmpty.NonEmpty TimeSeriesChunk
               , NonEmpty.NonEmpty TimeSeriesChunk
               , [Double])
             -> (NonEmpty.NonEmpty TimeSeriesChunk
               , NonEmpty.NonEmpty TimeSeriesChunk
               , NonEmpty.NonEmpty TimeSeriesChunk
               , [Double])
stlOuterLoop iNETSC interval (_seasonal, inTrend, _residuals, inRobustnessWeights) =
    let
        (seasonal, trend) = stlInnerLoop iNETSC interval inTrend inRobustnessWeights

        residuals = stlSubtract (stlSubtract iNETSC trend) seasonal
        robustnessWeights = calculateRobustnessWeights residuals
    in (seasonal, trend, residuals, robustnessWeights)

stlInnerLoop :: NonEmpty.NonEmpty TimeSeriesChunk
             -> Int
             -> NonEmpty.NonEmpty TimeSeriesChunk
             -> [Double]
             -> (NonEmpty.NonEmpty TimeSeriesChunk
               , NonEmpty.NonEmpty TimeSeriesChunk)
stlInnerLoop iNETSC interval initialTrend robustnessWeights =
    let
        seasonBandwith = 20.0
        trendBandwith = 3.5 * 86400.0 / fromIntegral interval

        robustFlippedLoess :: Double
                           -> NonEmpty.NonEmpty TimeSeriesChunk
                           -> NonEmpty.NonEmpty TimeSeriesChunk
        robustFlippedLoess bandwith tc = loess' tc bandwith robustnessWeights
    -- Step 1
        detrended = stlSubtract iNETSC initialTrend
    -- Step 2
        cycleSubSeries = splitByDay detrended
        smoothedCycleSubSeries = map (robustFlippedLoess seasonBandwith) cycleSubSeries
        stichedCycles = stitch smoothedCycleSubSeries
    -- Step 3
        lowPassSeasonal = loess'' stichedCycles 0.8
    -- Step 4
        seasonal = stlSubtract stichedCycles lowPassSeasonal
    -- Step 5
        deseasoned = stlSubtract iNETSC seasonal
    -- Step 6
        trend = robustFlippedLoess trendBandwith deseasoned
    in (seasonal, trend)

-- Subtract the second series from the first.
-- The result will have one and only one point for
-- each point in the left timeseries.
stlSubtract :: NonEmpty.NonEmpty TimeSeriesChunk
            -> NonEmpty.NonEmpty TimeSeriesChunk
            -> NonEmpty.NonEmpty TimeSeriesChunk
stlSubtract il ir =
    let il' = NonEmpty.toList il
        ir' = NonEmpty.toList ir
    in NonEmpty.fromList $ go il' ir'
    where
        go (_:ls) [] = def : go ls []
        go (l:ls) ar@(r:rs) = case tscTime l `compare` tscTime r of
            EQ -> subTimeSeriesChunk l r : go ls rs
            LT -> def : go ls ar
            GT -> go ls rs
        go [] _ = []

tricube :: Double
        -> Double
tricube x = ( 1 - x ** 3) ** 3

bisquare :: Double
         -> Double
bisquare x = if x < 1.0
                then ( 1 - x ** 2) ** 2
                else 0.0

linearLeastSquares' :: Integer
                    -> Double
                    -> V.Vector TimeSeriesChunk
                    -> [Double]
                    -> (Int, Double, Double, Double, Double, Double)
                    -> (Int, Double, Double, Double, Double, Double)
linearLeastSquares' v denom xandy _robustnessWeights (k, sw, sx, sxs, sy, sxy) =
    let xget = tscTime
        yget = tscAvg
        xk = xget (xandy V.! k)
        yk = yget (xandy V.! k)
        dist = abs $ v - xk
        --w = robustnessWeights !! k * (tricube $ fromIntegral dist * denom)
        w = (tricube $ fromIntegral dist * denom)
        xkw = fromIntegral xk * w
        sw' = sw + w
        sx' = sx + xkw
        sxs' = sxs + fromIntegral xk * xkw
        sy' = sy + yk * w
        sxy' = sxy + yk * xkw
        k' = k + 1
    in (k', sw' , sx', sxs', sy', sxy')

loess'' :: NonEmpty.NonEmpty TimeSeriesChunk
        -> Double
        -> NonEmpty.NonEmpty TimeSeriesChunk
loess'' xandy bandwith = loess' xandy bandwith $ repeat 1.0

loess :: NonEmpty.NonEmpty TimeSeriesChunk
      -> Double
      -> NonEmpty.NonEmpty TimeSeriesChunk
loess = loess''
--    let result = loess' xandy bandwith $ cycle [1.0]
--        right = fmap (divTimeSeriesChunkScalar 2.0) result
--    in stlSubtract result right

loess' :: NonEmpty.NonEmpty TimeSeriesChunk
       -> Double
       -> [Double]
       -> NonEmpty.NonEmpty TimeSeriesChunk
loess' xandy bandwith robustnessWeights =
    let
        left :: Int
        left = 0

        right :: Int
        right = if bandwith <= 1.0
                    then floor (bandwith * fromIntegral (NonEmpty.length xandy)) - 1
                    else minimum [NonEmpty.length xandy, floor bandwith] - 1

        xget = tscTime
        xandy' = V.fromList $ NonEmpty.toList xandy
        meat :: ([TimeSeriesChunk], Int, Int)
             -> (TimeSeriesChunk, Int)
             -> ([TimeSeriesChunk], Int, Int)
        meat (res, l, r) (vtsc,i)  =
            --v = xval !! i
            let v = xget vtsc
                (newl, newr) = if (i > 0) && (r < V.length xandy' - 1 && xget (xandy' V.! (r + 1)) - v < v - xget (xandy' V.! l))
                                  then (l+1, r+1)
                                  else (l, r)
                edge = if v - xget (xandy' V.! newl) > xget (xandy' V.! newr) - v
                          then newl
                          else newr
                denom :: Double
                denom = abs $ 1.0 / fromIntegral (xget (xandy' V.! edge) - v)

                closedLLS = linearLeastSquares' v denom xandy' robustnessWeights

                (_k,sumWeights, sumX, sumXSquared, sumY, sumXY) = iterate closedLLS (newl,0, 0,0,0,0) !! (newr - newl + 1)

                meanX = sumX / sumWeights
                meanY = sumY / sumWeights
                meanXY = sumXY / sumWeights
                meanXSquared = sumXSquared / sumWeights

                beta = if meanXSquared == meanX * meanX
                          then 0
                          else (meanXY - meanX * meanY) / (meanXSquared - meanX * meanX)
                alpha = meanY - beta * meanX
                newres = vtsc { tscAvg = beta * fromIntegral v + alpha }:res

            in (newres, newl, newr)
        mainResult :: [TimeSeriesChunk]
        (mainResult, _fl, _fr) = Fold.foldl' meat ([],left,right) (NonEmpty.zip xandy (NonEmpty.fromList [0..]))
    in NonEmpty.fromList $ reverse mainResult


insertElement :: MS.MultiSet LessTSC
              -> MS.MultiSet GreaterTSC
              -> TimeSeriesChunk
              -> (MS.MultiSet LessTSC, MS.MultiSet GreaterTSC)
insertElement mi ma e =
    let (nmi, nma) = if MS.null mi || tscAvg e < tscAvg (unLessTSC $ MS.findMin mi)
            then (mi, MS.insert (GreaterTSC e) ma)
            else (MS.insert (LessTSC e) mi, ma)
        (nmi', nma') = if MS.size nmi > MS.size nma + 1
            then let min@(LessTSC min') = MS.findMin nmi
                 in (MS.delete min nmi, MS.insert (GreaterTSC min') nma)
            else let max@(GreaterTSC max') = MS.findMin nma
                 in (MS.insert (LessTSC max') nmi, MS.delete max nma)
        (newmi, newma) = (nmi', nma')
    in (newmi, newma)

removeElement :: MS.MultiSet LessTSC
              -> MS.MultiSet GreaterTSC
              -> TimeSeriesChunk
              -> (MS.MultiSet LessTSC, MS.MultiSet GreaterTSC)
removeElement mi ma e =
    let (nmi, nma) = if tscAvg e < tscAvg (unLessTSC $ MS.findMin mi)
            then (mi, MS.delete (GreaterTSC e) ma)
            else (MS.delete (LessTSC e) mi, ma)
        (nmi', nma')
          | MS.size nmi > MS.size nma + 1 =
            let min@(LessTSC min') = MS.findMin nmi
            in (MS.delete min nmi, MS.insert (GreaterTSC min') nma)
          | MS.size nma > MS.size nmi + 1 =
            let max@(GreaterTSC max') = MS.findMin nma
            in (MS.insert (LessTSC max') nmi, MS.delete max nma)
          | otherwise = (nmi, nma)
        (newmi, newma) = (nmi', nma')
    in (newmi, newma)

getMedian :: MS.MultiSet LessTSC
          -> MS.MultiSet GreaterTSC
          -> TimeSeriesChunk
getMedian mi ma
  | MS.size mi > MS.size ma =
    let (LessTSC x) = MS.findMin mi
    in x
  | MS.size ma > MS.size mi =
    let (GreaterTSC x) = MS.findMin ma
    in x
  | otherwise =
    let (GreaterTSC l) = MS.findMin ma
        (LessTSC r) = MS.findMin mi
    in divTimeSeriesChunkScalar 2 $ addTimeSeriesChunk l r

linear :: Double
       -> Double
linear = const 1
constant :: Double
         -> Double
constant = const 0
quadratic :: Double
          -> Double
quadratic x = 2*x+1


data Penalty = Linear | Const | Quadratic

choosePenalty :: Penalty
              -> (Double -> Double)
choosePenalty x = case x of
    Linear -> linear
    Const -> constant
    Quadratic -> quadratic
while :: (a -> Bool)
      -> (a -> a)
      -> a
      -> a
while p = until (not . p)

-- transliteration
-- https://github.com/twitter/BreakoutDetection/blob/master/src/edm-multi.cpp
edmMulti :: NonEmpty.NonEmpty TimeSeriesChunk
         -> Int
         -> Double
         -> Penalty
         -> Maybe (NonEmpty.NonEmpty TimeSeriesChunk)
edmMulti ts minSize beta degree =
    let g = choosePenalty degree
        ts' = V.fromList $ NonEmpty.toList ts
        n = V.length ts'
        prev :: V.Vector Int
        prev = V.replicate (n+1) 0
        number :: V.Vector Int
        number = V.replicate (n+1) 0
        f :: V.Vector Double
        f = V.replicate (n+1) (-3)
        (_, _nums, _fss, prevs) = while (\(s,_,_,_) -> s < n + 1)
                                        withs
                                        (2*minSize, number, f, prev)
        withs :: (Int, V.Vector Int, V.Vector Double, V.Vector Int)
              -> (Int, V.Vector Int, V.Vector Double, V.Vector Int)
        withs (s, nms, fs, prs) =
            let
                rmin, lmin :: MS.MultiSet LessTSC
                rmax, lmax :: MS.MultiSet GreaterTSC
                rmax = MS.empty
                rmin = MS.empty
                lmax = MS.empty
                lmin = MS.empty
                (_, lmin', lmax') = while (\(i,_, _) -> i < minSize - 1)
                                          fill
                                          (prs V.! (minSize - 1), lmin, lmax)
                (_, rmin', rmax') = while (\(i,_, _) -> i < s)
                                          fill
                                          (minSize - 1, rmin, rmax)

                fill :: (Int, MS.MultiSet LessTSC, MS.MultiSet GreaterTSC)
                     -> (Int, MS.MultiSet LessTSC, MS.MultiSet GreaterTSC)
                fill (c, lm, rm) =
                        let (lm', rm') = insertElement lm rm (ts' V.! c)
                        in (c+1, lm', rm')

                (_, _lmin'', _lmax'', _rmin'', _rmax'', number', f', prev') =
                    while (\(t, _, _, _, _, _, _, _) -> t < s - minSize + 1)
                          witht
                          (minSize, lmin', lmax', rmin', rmax', nms, fs, prs)

                witht :: (Int, MS.MultiSet LessTSC, MS.MultiSet GreaterTSC, MS.MultiSet LessTSC, MS.MultiSet GreaterTSC, V.Vector Int, V.Vector Double, V.Vector Int)
                      -> (Int, MS.MultiSet LessTSC, MS.MultiSet GreaterTSC, MS.MultiSet LessTSC, MS.MultiSet GreaterTSC, V.Vector Int, V.Vector Double, V.Vector Int)
                witht (t, lm, lma, rm, rma, nm, fm, pr) =
                    let (lm', lma') = insertElement lm lma (ts' V.! (t - 1))
                        (rm', rma') = removeElement rm rma (ts' V.! (t - 1))
                        prElT = pr V.! t
                        prElT1 = pr V.! (t - 1)
                        (lm'', lma'')
                          | prElT > prElT1 =
                              let (_, lm''', lma''') = while (\(i, _, _) -> i < prElT)
                                                            act
                                                            (prElT1, lm', lma')
                                  act (ii, lml, lmal) =
                                    let (lml', lmal') = removeElement lml lmal (ts' V.! ii)
                                    in (ii+1, lml', lmal')
                              in (lm''', lma''')
                          | prElT < prElT1 =
                              let (_, lm''', lma''') = while (\(i, _, _) -> i < prElT1)
                                                            act
                                                            (prElT, lm', lma')
                                  act (ii, lml, lmal) =
                                    let (lml', lmal') = insertElement lml lmal (ts' V.! ii)
                                    in (ii+1, lml', lmal')
                              in (lm''', lma''')
                          | otherwise = (lm', lma')
                        leftMedian :: Double
                        leftMedian = tscAvg $ getMedian lm'' lma''
                        rightMedian :: Double
                        rightMedian = tscAvg $ getMedian rm' rma'

                        normalize :: Double
                        normalize =
                            let num = fromIntegral (( t - prElT ) * ( s - t))
                                denom = fromIntegral ((s - prElT) * (s - prElT))
                            in num / denom
                        tmp :: Double
                        tmp =
                            let fElT =  f V.! t
                                nmElT = nm V.! t
                                diffM = leftMedian - rightMedian
                                diffMS = diffM * diffM
                            in fElT + normalize * diffMS - beta * g (fromIntegral nmElT)
                    in if tmp > f V.! s
                          then let newPR = pr V.// [(s,t)]
                                   newFM = fm V.// [(s, tmp)]
                                   newNM = nm V.// [(s, (nm V.! t) + 1)]
                               in (t+1, lm'', lma'', rm', rma', newNM, newFM , newPR)
                          else (t+1, lm'', lma'', rm', rma', nm, fm, pr)
            in (s+1, number', f', prev')
        (_, ret) = while (\(at, _) -> at /= 0)
                         withat
                         (n, V.empty)
        withat (at, ret') =
            let at'' = prevs V.! at
                ret'' = if at'' /= 0 then V.snoc ret' at'' else ret'
            in (at'', ret'')
    in NonEmpty.nonEmpty $ map (\e -> ts' V.! e) $ L.sort $ V.toList ret
