{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Paradox.Istatd.CounterMapTest where


import Paradox.Istatd.CounterMap
import Paradox.Istatd.Types

import Data.List

import Test.Tasty
import Test.Tasty.TH

import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)

import qualified Data.Map.Strict as M

case_lookupSimple :: Assertion
case_lookupSimple = do
    let tsName = "key.to.find"
        tsFind = CounterSpec tsName Nothing
        cmap = make $ M.fromList [(CounterSpec "key.to.find" Nothing, 1 :: Int), (CounterSpec "key.not.to.find" Nothing, 2)]
        res = map snd $ unpackSearch $ deMaybe $ concatMap (flip lookupMagic cmap . convertForSearch) [tsFind]


    assertEqual "Found the correct key" [1] res

case_lookup :: Assertion
case_lookup = do
    let tsName = "key.to.find"
        tsFind = CounterSpec tsName Nothing
        cmap = make $ M.fromList [(CounterSpec "key.to.find" Nothing, 1 :: Int), (CounterSpec "key.not.to.find" Nothing, 2)]
        res = map snd $ concatMap (`lookupC` cmap) [tsFind]


    assertEqual "Found the correct key" [1] res

case_lookupMany :: Assertion
case_lookupMany = do
    let tsName = "key.to.find"
        tsFind = CounterSpec tsName Nothing
        cmap = make $ M.fromList [(CounterSpec "key.to.find" Nothing, 1 :: Int), (CounterSpec "key.not.to.find" Nothing, 2)]
        res = map snd $ (`lookupManyC` cmap) [tsFind]


    assertEqual "Found the correct key" [1] res

case_lookupSingle :: Assertion
case_lookupSingle = do
    let tsName = "key.to.find"
        tsFind = CounterSpec tsName Nothing
        cmap = make $ M.fromList [(CounterSpec "key.to.find" Nothing, 1 :: Int), (CounterSpec "key.not.to.find" Nothing, 2)]
        res = map snd $ lookupC tsFind cmap


    assertEqual "Found the correct key" [1] res

case_lookupSimpleGlob :: Assertion
case_lookupSimpleGlob = do
    let tsName = "key.to.*"
        tsFind = CounterSpec tsName Nothing
        cmap = make $ M.fromList [
                          (CounterSpec "key.to.find" Nothing, 1 :: Int)
                        , (CounterSpec "key.not.to.find" Nothing, 2)
                        , (CounterSpec "key.to.also.find" Nothing, 3)]
        res = map snd $ lookupC tsFind cmap


    assertEqual "Found the correct key" [1] (sort res)

case_lookupMultiGlob :: Assertion
case_lookupMultiGlob = do
    let tsName = "*.to.*"
        tsFind = CounterSpec tsName Nothing
        cmap = make $ M.fromList [
                          (CounterSpec "key.to.find" Nothing, 1 :: Int)
                        , (CounterSpec "key.not.find" Nothing, 2)
                        , (CounterSpec "key.to.also.find" Nothing, 3)
                        , (CounterSpec "and.to.also.find" Nothing, 4)
                        , (CounterSpec "and.an.to.also.find" Nothing, 5)]
        res = map snd $ lookupC tsFind cmap


    assertEqual "Found the correct key" [1] (sort res)

case_lookupPartialGlob :: Assertion
case_lookupPartialGlob = do
    let tsName = "key.to.find*"
        tsFind = CounterSpec tsName Nothing
        cmap = make $ M.fromList [
                          (CounterSpec "key.to.find" Nothing, 1 :: Int)
                        , (CounterSpec "key.to.find2" Nothing, 2)
                        , (CounterSpec "key.to.find3" Nothing, 3)
                        , (CounterSpec "key.to.find4" Nothing, 4)
                        , (CounterSpec "key.not.find" Nothing, 5)
                        , (CounterSpec "key.to.also.find" Nothing, 6)
                        , (CounterSpec "and.to.also.find" Nothing, 7)
                        , (CounterSpec "and.an.to.also.find" Nothing, 8)]
        res = map snd $ lookupC tsFind cmap


    assertEqual "Found the correct key" [1,2,3,4] (sort res)

case_lookupMultiplePartialGlob :: Assertion
case_lookupMultiplePartialGlob = do
    let tsName = "key.*o*.find*"
        tsFind = CounterSpec tsName Nothing
        cmap = make $ M.fromList [
                          (CounterSpec "key.to.find" Nothing, 1 :: Int)
                        , (CounterSpec "key.to.find2" Nothing, 2)
                        , (CounterSpec "key.to.find3" Nothing, 3)
                        , (CounterSpec "key.to.find4" Nothing, 4)
                        , (CounterSpec "key.not.find" Nothing, 5)
                        , (CounterSpec "key.to.also.find" Nothing, 6)
                        , (CounterSpec "and.to.also.find" Nothing, 7)
                        , (CounterSpec "and.an.to.also.find" Nothing, 8)]
        res = map snd $ lookupC tsFind cmap


    assertEqual "Found the correct key" [1,2,3,4,5] (sort res)



case_lookupSingleCharGlob :: Assertion
case_lookupSingleCharGlob = do
    let tsName = "key.to.?ind"
        tsFind = CounterSpec tsName Nothing
        cmap = make $ M.fromList [
                          (CounterSpec "key.to.find" Nothing, 1 :: Int)
                        , (CounterSpec "key.not.to.find" Nothing, 2)
                        , (CounterSpec "key.to.also.find" Nothing, 3)
                        , (CounterSpec "key.to.xind" Nothing, 4)
                        , (CounterSpec "key.to.mrind" Nothing, 5)
                        , (CounterSpec "key.to.xind.har" Nothing, 6)
                        , (CounterSpec "key.to.mrind.har" Nothing, 7)]
        res = map snd $ lookupC tsFind cmap


    assertEqual "Found the correct key" [1,4] (sort res)

case_lookupSingleCharAndMultiCharGlob :: Assertion
case_lookupSingleCharAndMultiCharGlob = do
    let tsName = "key.*.?ind"
        tsFind = CounterSpec tsName Nothing
        cmap = make $ M.fromList [
                          (CounterSpec "key.to.find" Nothing, 1 :: Int)
                        , (CounterSpec "key.not.to.find" Nothing, 2)
                        , (CounterSpec "key.to.also.find" Nothing, 3)
                        , (CounterSpec "key.to.xind" Nothing, 4)
                        , (CounterSpec "key.to.mrind" Nothing, 5)
                        , (CounterSpec "key.to.xind.har" Nothing, 6)
                        , (CounterSpec "key.to.mrind.har" Nothing, 7)]
        res = map snd $ lookupC tsFind cmap


    assertEqual "Found the correct key" [1,4] (sort res)

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
