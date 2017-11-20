{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -w #-}
module Language.Paradox.EvalTest where

import Test.Tasty
import Test.Tasty.TH

import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertFailure)

import Language.Paradox.Grammar
import Language.Paradox.Eval
import Language.Paradox.Util
import Text.Megaparsec

import Paradox.Shell.Types

import qualified Data.Map.Strict      as M
import qualified Data.Set      as S
import Paradox.Istatd.Types
import Language.Paradox.Eval.Types
import Paradox.Istatd.CounterMap
import Paradox.Istatd.Functions (finalizeIntermediateCounterSpec)

import Paradox.Debug
import Data.Default
import Data.Monoid (mempty,mappend)
import qualified Data.List.NonEmpty as NonEmpty
import Language.Paradox.GrammarCases

mkCtrsEnv ctrs = mkCtrsEnv' (map (\x -> (id,x)) ctrs) [1..6]
mkCtrsEnvMod ctrs = mkCtrsEnv' ctrs [1..6]

mkCtrsEnv' ctrs d = M.fromList
          . map (`mkCtrEnv` d) $ ctrs

mkCtrEnv (m, x@(CounterSpec n _)) d =
   ( convertForSearch x
   , def { ptsData = NonEmpty.fromList $ m $ map (\x' ->
       def {tscAvg = x' }
       ) d
       , ptsName = n
   })

case_ap_id = do
    let (res,accum) = evalWithEnv env expr
        expr = Apply Id (Counter ctrs)
        env = def { wMap = mkCtrsEnv ctrs }
        ctrs = map (`CounterSpec` Nothing) ["tep.tep", "a.counter", "a.200"]
    assertEqual "Data is unchanged" (M.fromList $ unpackSearch $ M.toList $ wMap env) (timeSeriesMap res)
    assertEqual "Logs"
                Accum { unLogs = [ LogEntry "apply" Bind
                               , LogEntry "id" Func
                               , LogEntry "counters" Lit
                                ]
                      }
                accum

case_pipe_id = do
    let (res,accum) = evalWithEnv env expr
        expr = Counter ctrs :|: Id
        env = def { wMap = mkCtrsEnv ctrs }
        ctrs = map (`CounterSpec` Nothing) ["tep.tep", "a.counter", "a.200"]
    assertEqual "Data is unchanged" (M.fromList $ unpackSearch $ M.toList $ wMap env) (timeSeriesMap res)
    assertEqual "Logs"
                Accum { unLogs = [ LogEntry "infix_apply" Bind
                               , LogEntry "counters" Lit
                               , LogEntry "id" Func
                               ]
                      }
                accum

case_pipe_chain_and_sum = do
    let (res,accum) = evalWithEnv env expr
        compositeName = "sum(a.200,a.counter,tep.tep)"
        exp = M.fromList [ (CounterSpec compositeName Nothing
                         , def { ptsName = compositeName
                               , ptsData = NonEmpty.fromList $ map (\x' -> def {tscAvg = x' }) [3,6,9,12,15,18]
                               }
                           )
                         ]
        expr = Counter ctrs :|: Id :|: Sum
        env = def { wMap = mkCtrsEnv ctrs }
        ctrs = map (`CounterSpec` Nothing) ["tep.tep", "a.counter", "a.200"]
    assertEqual "Data is summed" exp (timeSeriesMap res)
    assertEqual "Logs"
                Accum { unLogs = [ LogEntry "infix_apply" Bind
                               , LogEntry "infix_apply" Bind
                               , LogEntry "counters" Lit
                               , LogEntry "id" Func
                               , LogEntry "sum" Func
                               ]
                      }
                accum

case_sub_infix_sum_expr = do
    let (res,accum) = evalWithEnv env expr
        compositeName = "subtract(sum(a.200,a.counter,tep.tep),sub.values)"
        exp = M.fromList [ (CounterSpec compositeName Nothing
                         , def { ptsName = compositeName
                               , ptsData = NonEmpty.fromList $ map (\x' -> def {tscAvg = x' }) [2,4,6,8,10,12]
                               }
                           )
                         ]
        expr = Apply (Apply Sub (Counter ctrs :|: Id :|: Sum)) (Counter ctrs')

        env = def { wMap = mkCtrsEnv ctrs `mappend` mkCtrsEnv ctrs' }
        ctrs = map (`CounterSpec` Nothing) ["tep.tep", "a.counter", "a.200"]
        ctrs' = map (`CounterSpec` Nothing) ["sub.values"]
    assertEqual "Data is summed" exp (timeSeriesMap res)
    assertEqual "Logs"
                Accum { unLogs = [ LogEntry "apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "subtract" Func
                               , LogEntry "infix_apply" Bind
                               , LogEntry "infix_apply" Bind
                               , LogEntry "counters" Lit
                               , LogEntry "id" Func
                               , LogEntry "sum" Func
                               , LogEntry "counters" Lit
                               ]
                      }
                accum

case_sub_infix_sum_expr_w_alias = do
    let (res,accum) = evalWithEnv env expr
        compositeName = "A New Name"
        exp = M.fromList [ (CounterSpec compositeName Nothing
                         , def { ptsName = compositeName
                               , ptsData = NonEmpty.fromList $ map (\x' -> def {tscAvg = x' }) [2,4,6,8,10,12]
                               }
                           )
                         ]
        expr = Apply (Apply Sub (Counter ctrs :|: Id :|: Sum)) (Counter ctrs') :|: Apply Alias (CounterN compositeName)

        env = def { wMap = mkCtrsEnv ctrs `mappend` mkCtrsEnv ctrs' }
        ctrs = map (`CounterSpec` Nothing) ["tep.tep", "a.counter", "a.200"]
        ctrs' = map (`CounterSpec` Nothing) ["sub.values"]
    assertEqual "Data is summed" exp (timeSeriesMap res)
    assertEqual "Logs"
                Accum { unLogs = [ LogEntry "infix_apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "subtract" Func
                               , LogEntry "infix_apply" Bind
                               , LogEntry "infix_apply" Bind
                               , LogEntry "counters" Lit
                               , LogEntry "id" Func
                               , LogEntry "sum" Func
                               , LogEntry "counters" Lit
                               , LogEntry "apply" Bind
                               , LogEntry "alias" Func
                               , LogEntry "counter_name" Lit
                                ]
                      }
                accum

case_compose = do
    let (res,accum) = evalWithEnv env expr
        compositeName = "A New Name"
        exp = M.fromList [ (CounterSpec compositeName Nothing
                         , def { ptsName = compositeName
                               , ptsData = NonEmpty.fromList $ map (\x' -> def {tscAvg = x' }) [2,4,6,8,10,12]
                               , ptsDrawOptions = MappedOptions (M.fromList [("Thing",DrawOptionB True),("Thing2",DrawOptionS "Value")])
                               }
                           )
                         ]
        expr = Apply (Apply Sub (Counter ctrs :|: Id :|: Sum)) (Counter ctrs') :|: (Apply (Apply GraphOptB (CounterN "Thing")) (BoolC True) :.: Apply (Apply GraphOptS (CounterN "Thing2")) (CounterN "Value")) :|: Apply Alias (CounterN compositeName)

        env = def { wMap = mkCtrsEnv ctrs `mappend` mkCtrsEnv ctrs' }
        ctrs = map (`CounterSpec` Nothing) ["tep.tep", "a.counter", "a.200"]
        ctrs' = map (`CounterSpec` Nothing) ["sub.values"]
    assertEqual "Data is summed" exp (timeSeriesMap res)
    assertEqual "Logs"
                Accum { unLogs = [ LogEntry "infix_apply" Bind
                               , LogEntry "infix_apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "subtract" Func
                               , LogEntry "infix_apply" Bind
                               , LogEntry "infix_apply" Bind
                               , LogEntry "counters" Lit
                               , LogEntry "id" Func
                               , LogEntry "sum" Func
                               , LogEntry "counters" Lit
                               , LogEntry "compose" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "graph_options_bool" Pass
                               , LogEntry "counter_name" Lit
                               , LogEntry "bool" Lit
                               , LogEntry "apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "graph_options_string" Pass
                               , LogEntry "counter_name" Lit
                               , LogEntry "counter_name" Lit
                               , LogEntry "apply" Bind
                               , LogEntry "alias" Func
                               , LogEntry "counter_name" Lit
                                ]
                      }
                accum

case_higher_order_map = do
    let (res,accum) = evalWithEnv env expr
        exp = M.fromList [
                           (CounterSpec "tep.tep" Nothing
                         , def { ptsName = "tep.tep"
                               , ptsData = NonEmpty.fromList $ map (\x' -> def {tscAvg = x' }) [1,1,1,2,2,2,2,2,2,2,2,2,2,2]
                               }
                           )
                         ,
                           (CounterSpec "a.counter" Nothing
                         , def { ptsName = "a.counter"
                               , ptsData = NonEmpty.fromList $ map (\x' -> def {tscAvg = x' }) [2..20]
                               }
                           )
                         ,
                           (CounterSpec "a.200" Nothing
                         , def { ptsName = "a.200"
                               , ptsData = NonEmpty.fromList $ map (\x' -> def {tscAvg = x' }) [1..10]
                               }
                           )
                         ]
        expGraphOpts = MappedOptions (M.fromList [("constrainMax",DrawOptionD 10.0),("constrainMin",DrawOptionD 0.0)])
        --expr = Apply (Apply Sub (Counter ctrs :|: Id :|: Sum)) (Counter ctrs') :|: ((Apply (Apply GraphOptB (CounterN "Thing")) (BoolC True)) :.: (Apply (Apply GraphOptS (CounterN "Thing2")) (CounterN "Value"))) :|: Apply Alias (CounterN compositeName)

        expr = (:|:)
                    (Counter $ map snd ctrs)
                    (Apply Map (Apply (Apply (Apply WhenElse (Apply (Apply (Apply HasAnyValue GreaterThan) GetAvg) (NumD 2.5))) (Apply (Apply ApplyWithValue (Apply SurfaceOptD (CounterN "constrainMax"))) ((:.:) (Apply (Apply GetLargestInRange GetAvg) (NumD 10.0)) (Apply LargerD (NumD 2.5))))) (Apply (Apply ((:...:) (Apply SurfaceOptD (CounterN "constrainMin")) (Apply SurfaceOptD (CounterN "constrainMax"))) (NumD 0.0)) (NumD 2.5))))
        env = def { wMap = M.fromList [mkCtrEnv (head ctrs) [1,1,1,2,2,2,2,2,2,2,2,2,2,2], mkCtrEnv (ctrs !! 1) [2..20], mkCtrEnv (ctrs !! 2) [1..10]] }
        ctrs = map (\x -> (id, x `CounterSpec` Nothing)) ["tep.tep", "a.counter", "a.200"]
    assertEqual "Data is summed" exp (timeSeriesMap res)
    assertEqual "GraphOpts" expGraphOpts (options res)
    assertEqual "Logs"
                Accum { unLogs = [ LogEntry "infix_apply" Bind
                               , LogEntry "counters" Lit
                               , LogEntry "apply" Bind
                               , LogEntry "map" HigherOrder
                               , LogEntry "apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "when_else" HigherOrder
                               , LogEntry "apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "has_any_value" HigherOrder
                               , LogEntry "greater_than" HigherOrder
                               , LogEntry "get_avg" HigherOrder
                               , LogEntry "num_double" Lit
                               , LogEntry "apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "apply_with_value" HigherOrder
                               , LogEntry "apply" Bind
                               , LogEntry "surface_options_double" Pass
                               , LogEntry "counter_name" Lit
                               , LogEntry "compose" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "get_largest_in_range" HigherOrder
                               , LogEntry "get_avg" HigherOrder
                               , LogEntry "num_double" Lit
                               , LogEntry "apply" Bind
                               , LogEntry "larger_double" HigherOrder
                               , LogEntry "num_double" Lit
                               , LogEntry "apply" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "chain" Bind
                               , LogEntry "apply" Bind
                               , LogEntry "surface_options_double" Pass
                               , LogEntry "counter_name" Lit
                               , LogEntry "apply" Bind
                               , LogEntry "surface_options_double" Pass
                               , LogEntry "counter_name" Lit
                               , LogEntry "num_double" Lit
                               , LogEntry "num_double" Lit
                                ]
                      }
                accum

case_resolve_internal = do
    let ifs = ["xe-0_0_0", "xe-0_0_1", "xe-0_0_2", "xe-0_0_3"]
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
        Counter vals = results

        genCounter i h =
                    [ SCNPString "network"
                    , SCNPDot
                    , SCNPString "interface"
                    , SCNPDot
                    , SCNPShellReplaced i
                    , SCNPDot
                    , SCNPString "octets"
                    , SCNPDot
                    , SCNPGlobStar
                    , SCNPDot
                    , SCNPString "host"
                    , SCNPDot
                    , SCNPShellReplaced h
                    ]
        expectation
          = S.fromList
          $ finalizeIntermediateCounterSpec
            csi
            [genCounter i h | i <- ifs, h <- hosts]
    assertEqual "Matrix of 8 results" 8 (length $ traceIt "\nWhat did we get back\n" vals)
    assertEqual "Expected counter list generated" expectation (S.fromList vals)

case_include = do
    let (res,accum) = evalWithEnv env expr
        exp = M.fromList [ (CounterSpec "a.200" Nothing
                         , def { ptsName = "a.200"
                               , ptsData = NonEmpty.fromList $ map (\x' -> def {tscAvg = x' }) [1,2,3,4,5,6]
                               }
                           )
                           , (CounterSpec "a.counter" Nothing
                         , def { ptsName = "a.counter"
                               , ptsData = NonEmpty.fromList $ map (\x' -> def {tscAvg = x' }) [1,2,3,4,5,6]
                               }
                           )
                         ]
        expr = Counter ctrs :|: Apply Include (CounterN "a[.]")
        env = def { wMap = mkCtrsEnv ctrs }
        ctrs = map (`CounterSpec` Nothing) ["tep.tep", "a.counter", "a.200"]
    assertEqual "Data is summed" exp (timeSeriesMap res)
    assertEqual "Logs"
                Accum { unLogs = [ LogEntry "infix_apply" Bind
                               , LogEntry "counters" Lit
                               , LogEntry "apply" Bind
                               , LogEntry "include" Func
                               , LogEntry "counter_name" Lit
                               ]
                      }
                accum

case_graph_count_field = do
    let (res,accum) = evalWithEnv env expr
        compositeName = "graphCount(tep.tep)"
        exp = M.fromList [ (CounterSpec compositeName Nothing
                         , def { ptsName = compositeName
                               , ptsData = NonEmpty.fromList $ map (\(x',c') -> def {tscAvg = fromIntegral c', tscCount = c', tscMax = fromIntegral c', tscMin = fromIntegral c' }) $ zip [1,2,3,4,5,6] [6,5,4,3,2,1]
                               }
                           )
                         ]
        expr = Counter (map snd ctrs) :|: (GetCount :|: GraphField)
        env = def { wMap = mkCtrsEnvMod ctrs }
        ctrs = [ctr]
        ctr = (modi, (`CounterSpec` Nothing) "tep.tep")
        modi tscs = map (\(x,c) -> x { tscCount = c} ) $ zip tscs [6,5..1]
    assertEqual "Data is summed" exp (timeSeriesMap res)
    assertEqual "Logs"
                Accum { unLogs = [ LogEntry "infix_apply" Bind
                               , LogEntry "counters" Lit
                               , LogEntry "infix_apply" Bind
                               , LogEntry "get_count" HigherOrder
                               , LogEntry "graph_field" Func
                               ]
                      }
                accum


tests = $(testGroupGenerator)

main = defaultMain tests
