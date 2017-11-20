{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Paradox.Istatd.CounterTreeTest where

import Paradox.Istatd.CounterTree

import Test.Tasty
import Test.Tasty.TH

import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)
import Data.Text (Text)

import Data.HashMap.Strict (fromList, HashMap)

test :: SuperHashMap
test = construct
  [ ( ["asdb", "imvu", "xy_num_connected_user"], IValue True  Gauge )
  , ( ["asdb", "imvu", "messages"]             , IValue True  Counter )
  , ( ["asdb", "imvu", "forward"]              , IValue False Aggregate )
  , ( ["asdb", "withme", "com"]                , IValue False Aggregate )
  , ( ["mobile"]                              , IValue False Aggregate )
  ]

testDeep :: SuperHashMap
testDeep = construct
  [ ( ["asdb", "imvu", "xy_num_connected_user", "host"], IValue False Aggregate )
  , ( ["asdb", "imvu", "xy_num_connected_user", "asdb-gateway"], IValue True  Counter )
  , ( ["asdb", "imvu", "xy_num_connected_user", "host", "host12340"], IValue True  Gauge )
  , ( ["asdb", "imvu", "xy_num_connected_user", "host", "host12240"], IValue True  Gauge )
  , ( ["asdb", "imvu", "xy_num_connected_user", "host", "host12210"], IValue True  Gauge )
  , ( ["asdb", "imvu", "messages"]             , IValue True  Aggregate )
  , ( ["asdb", "imvu", "forward"]              , IValue False Aggregate )
  , ( ["asdb", "withme", "com"]                , IValue False Aggregate )
  , ( ["mobile"]                              , IValue False Aggregate )
  ]

case_lookup_many_finds_no_values_but_gets_subtree :: Assertion
case_lookup_many_finds_no_values_but_gets_subtree = do
    let ex :: Maybe ([(Text, Maybe IValue)], HashMap Text (Maybe IValue, SuperHashMap))
        ex =  Just
                (
                  [
                        ("imvu",Nothing)
                      , ("asdb",Nothing)
                  ]
                , fromList [
                      ("xy_num_connected_user",(Just IValue {isLeaf = True, _counterType = Gauge},SuperHashMap (fromList [])))
                    , ("forward",(Just IValue {isLeaf = False, _counterType = Aggregate},SuperHashMap (fromList [])))
                    , ("messages",(Just IValue {isLeaf = True, _counterType = Counter},SuperHashMap (fromList [])))
                  ]
                )

        res :: Maybe ([(Text, Maybe IValue)], HashMap Text (Maybe IValue, SuperHashMap))
        res = lookupMany ["asdb", "imvu"] test

    assertEqual "Looked up tree from test SuperHashMap" ex res

case_lookup_many_finds_values :: Assertion
case_lookup_many_finds_values = do
    let ex :: Maybe ([(Text, Maybe IValue)], HashMap Text (Maybe IValue, SuperHashMap))
        ex =  Just
                (
                  [
                        ("xy_num_connected_user",Just IValue {isLeaf = True, _counterType = Gauge})
                      , ("imvu",Nothing)
                      , ("asdb",Nothing)
                  ]
                , fromList [
                  ]
                )

        res :: Maybe ([(Text, Maybe IValue)], HashMap Text (Maybe IValue, SuperHashMap))
        res = lookupMany ["asdb", "imvu", "xy_num_connected_user"] test

    assertEqual "Looked up value from test SuperHashMap" ex res

case_lookup_many_finds_no_values_when_missing :: Assertion
case_lookup_many_finds_no_values_when_missing = do
    let ex :: Maybe ([(Text, Maybe IValue)], HashMap Text (Maybe IValue, SuperHashMap))
        ex =  Nothing

        res :: Maybe ([(Text, Maybe IValue)], HashMap Text (Maybe IValue, SuperHashMap))
        res = lookupMany ["asdb", "imvu", "xy_app"] test

    assertEqual "Looked up value from test SuperHashMap" ex res

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
