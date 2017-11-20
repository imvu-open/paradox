{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}
module Paradox.EvalTest where

import Test.Tasty
import Test.Tasty.TH

import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertFailure)

import Data.Monoid

import Paradox.Shell.Types
import Language.Paradox.Eval
import Paradox.Eval
import Paradox.Eval.Types
import Language.Paradox.Eval.Types
import Paradox.Istatd.Types
import Paradox.Types
import Paradox.Istatd.CounterMap

import qualified Data.Map.Strict as M


case_single_keys_query_with_include = do
    let cev = [CEvaluable "ctr.* | include \"123\"" (Counter [CounterSpec "ctr.*" Nothing] :|: Apply Include (CounterN "123"))]
        csmap = make $ M.fromList [(CounterSpec "ctr.123" Nothing, CountersMatch True 1 "ctr.123"), (CounterSpec "ctr.234" Nothing, CountersMatch True 1 "ctr.234")]
        map = CounterMapCache csmap (CountersTypeMapping mempty) Nothing
        keys = keysFromQueries map cev

        exp = [CounterSpec "ctr.123" Nothing]
    assertEqual "Got 1 key" exp keys

case_multiple_keys_query_with_include = do
    let cev = [ CEvaluable "ctr.* | include \"123\"" (Counter [CounterSpec "ctr.*" Nothing] :|: Apply Include (CounterN "123"))
              , CEvaluable "ctr.* | exclude \"234\"" (Counter [CounterSpec "ctr.*" Nothing] :|: Apply Exclude (CounterN "234"))
              ]
        csmap = make $ M.fromList [(CounterSpec "ctr.123" Nothing, CountersMatch True 1 "ctr.123"), (CounterSpec "ctr.234" Nothing, CountersMatch True 1 "ctr.234")]
        map = CounterMapCache csmap (CountersTypeMapping mempty) Nothing
        keys = keysFromQueries map cev

        exp = [CounterSpec "ctr.123" Nothing]
    assertEqual "Got 1 keys" exp keys

case_multiple_keys_query_with_conflicted = do
    let cev = [ CEvaluable "ctr.* | include \"123\"" (Counter [CounterSpec "ctr.*" Nothing] :|: Apply Include (CounterN "123"))
              , CEvaluable "ctr.* | exclude \"123\"" (Counter [CounterSpec "ctr.*" Nothing] :|: Apply Exclude (CounterN "123"))
              ]
        csmap = make $ M.fromList [(CounterSpec "ctr.123" Nothing, CountersMatch True 1 "ctr.123"), (CounterSpec "ctr.234" Nothing, CountersMatch True 1 "ctr.234")]
        map = CounterMapCache csmap (CountersTypeMapping mempty) Nothing
        keys = keysFromQueries map cev

        exp = [CounterSpec "ctr.123" Nothing, CounterSpec "ctr.234" Nothing]
    assertEqual "Got 2 keys" exp keys

case_key_not_in_cache_glob = do
    let
      ifs = ["xe-0_0_0", "xe-0_0_1", "xe-0_0_2", "xe-0_0_3"]
      hosts = ["HOST1234", "HOST1235"]
      csi = CounterSpecIntermediate
              [ CNPString "network"
              , CNPDot
              , CNPString "interface"
              , CNPDot
              , CNPShellReplacement "echo xe-0_0_0 xe-0_0_1 xe-0_0_2 xe-0_0_3"
              , CNPDot
              , CNPString "octets"
              , CNPDot
              , CNPGlobStar
              , CNPDot
              , CNPString "host"
              , CNPDot
              , CNPShellReplacement "echo HOST1234 HOST1235"
              ]
              Nothing
      successes = [ SandboxedActionSuccess (UnsafeSandboxedAction "echo" ifs, ifs)
                  , SandboxedActionSuccess (UnsafeSandboxedAction "echo" hosts, hosts)
                  ]
      csis = [csi]
      Right results = resolveInternal csis [] successes
    let
      cev = [ CEvaluable
                "network.interface.{'echo xe-0_0_0 xe-0_0_1 xe-0_0_2 xe-0_0_3'}.octets.host.{'echo HOST1234 HOST1235'}"
                results
            ]
      csmap = make
            $ M.fromList
              [ ( CounterSpec "network.interface.xe-0_0_0.octets.umlaut.host.host1234" Nothing
                , CountersMatch True 1 "network.interface.xe-0_0_0.octets.umlaut.host.host1234"
                )
              ]
      map = CounterMapCache csmap (CountersTypeMapping mempty) Nothing
      keys = keysFromQueries map cev

      exp = [CounterSpec "network.interface.xe-0_0_0.octets.umlaut.host.host1234" Nothing]
    assertEqual "Got 1 keys" exp keys

case_key_not_in_cache = do
    let
      ifs = ["xe-0_0_0", "xe-0_0_1", "xe-0_0_2", "xe-0_0_3"]
      hosts = ["HOST1234", "HOST1235"]
      csi = CounterSpecIntermediate
              [ CNPString "network"
              , CNPDot
              , CNPString "interface"
              , CNPDot
              , CNPShellReplacement "echo xe-0_0_0 xe-0_0_1 xe-0_0_2 xe-0_0_3"
              , CNPDot
              , CNPString "octets"
              , CNPDot
              , CNPString "host"
              , CNPDot
              , CNPShellReplacement "echo HOST1234 HOST1235"
              ]
              Nothing
      successes = [ SandboxedActionSuccess (UnsafeSandboxedAction "echo" ifs, ifs)
                  , SandboxedActionSuccess (UnsafeSandboxedAction "echo" hosts, hosts)
                  ]
      csis = [csi]
      Right results = resolveInternal csis [] successes
    let
      cev = [ CEvaluable
                "network.interface.{'echo xe-0_0_0 xe-0_0_1 xe-0_0_2 xe-0_0_3'}.octets.host.{'echo HOST1234 HOST1235'}"
                results
            ]
      csmap = make
            $ M.fromList
              [ ( CounterSpec "network.interface.xe-0_0_0.octets.host.host1234" Nothing
                , CountersMatch True 1 "network.interface.xe-0_0_0.octets.host.host1234"
                )
              ]
      map = CounterMapCache csmap (CountersTypeMapping mempty) Nothing
      keys = keysFromQueries map cev

      exp = [CounterSpec "network.interface.xe-0_0_0.octets.host.host1234" Nothing]
    assertEqual "Got 1 keys" exp keys


tests = $(testGroupGenerator)

main = defaultMain tests
