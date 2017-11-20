{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -w #-}
module Language.Paradox.GrammarTest where

import Data.Monoid ((<>))
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
import qualified Paradox.Istatd.Types as IT

import Paradox.Debug
import Language.Paradox.GrammarCases


check_grammar_parsed = isOk . runParser expr ""
    where
        isOk (Right _) = return ()
        isOk (Left err) = assertFailure $ "Bad parse" <> parseErrorPretty err

case_simple_infix_grammar                      = check_grammar_parsed check_simple_infix_grammar
case_simple_ap_grammar                         = check_grammar_parsed check_simple_ap_grammar
case_simple_infix_grammar_multi_space          = check_grammar_parsed check_simple_infix_grammar_multi_space
case_simple_ap_grammar_multi_space             = check_grammar_parsed check_simple_ap_grammar_multi_space
case_simple_grammar_spaces_around_parens       = check_grammar_parsed check_simple_grammar_spaces_around_parens
case_infix_grammar_spaces_around_parens        = check_grammar_parsed check_infix_grammar_spaces_around_parens
case_spaces_and_parens                         = check_grammar_parsed check_spaces_and_parens
case_space_at_end                              = check_grammar_parsed check_space_at_end
case_space_at_start                            = check_grammar_parsed check_space_at_start
case_dumb_counter_names_grammar                = check_grammar_parsed check_dumb_counter_names_grammar
case_scale_grammar_w_negative                  = check_grammar_parsed check_scale_grammar_w_negative
case_arrity_2_infix_grammar                    = check_grammar_parsed check_arrity_2_infix_grammar
case_parens_grammar_into_infix                 = check_grammar_parsed check_parens_grammar_into_infix
case_parens_grammar_with_ap                    = check_grammar_parsed check_parens_grammar_with_ap
case_parens_grammar_with_ap_and_infix          = check_grammar_parsed check_parens_grammar_with_ap_and_infix
case_properties_grammar                        = check_grammar_parsed check_properties_grammar
case_properties_grammar_with_infix             = check_grammar_parsed check_properties_grammar_with_infix
case_negative_value_in_properties              = check_grammar_parsed check_negative_value_in_properties
case_simple_alias                              = check_grammar_parsed check_simple_alias
case_simple_globs                              = check_grammar_parsed check_simple_globs
case_partial_globs                             = check_grammar_parsed check_partial_globs
case_partial_globs2                            = check_grammar_parsed check_partial_globs2
case_simple_question_marks                     = check_grammar_parsed check_simple_question_marks
case_array_counter_notation                    = check_grammar_parsed check_array_counter_notation
case_globs_and_string_properties               = check_grammar_parsed check_globs_and_string_properties
case_parens_and_string_properties              = check_grammar_parsed check_parens_and_string_properties
case_extra_properties                          = check_grammar_parsed check_extra_properties
case_heavy_nesting                             = check_grammar_parsed check_heavy_nesting
case_offset_followed_by_nesting                = check_grammar_parsed check_offset_followed_by_nesting
case_complex_offset_time_range                 = check_grammar_parsed check_complex_offset_time_range
case_constant_line                             = check_grammar_parsed check_constant_line
case_constant_line_and_cons                    = check_grammar_parsed check_constant_line_and_cons
case_constant_line_and_cons_w_parens           = check_grammar_parsed check_constant_line_and_cons_w_parens
case_negative_scale                            = check_grammar_parsed check_negative_scale
case_shell_escaping                            = check_grammar_parsed check_shell_escaping
case_globbing_and_aliasing                     = check_grammar_parsed check_globbing_and_aliasing
case_globbing_and_aliasing_with_infix_operator = check_grammar_parsed check_globbing_and_aliasing_with_infix_operator
case_aliasing                                  = check_grammar_parsed check_aliasing
case_aliasing_with_infix_operator              = check_grammar_parsed check_aliasing_with_infix_operator
case_grammarWebResponse                        = check_grammar_parsed check_grammarWebResponse
case_grammarWebResponseA                       = check_grammar_parsed check_grammarWebResponseA
case_grammar_conditional_constraints           = check_grammar_parsed check_grammar_conditional_constraints
case_uppercase                                  = check_grammar_parsed check_uppercase

case_expand_parts =
    assertEqual "" [[CtrLit "cpu",CtrDot,CtrLit "0",CtrDot,CtrLit "idle",CtrDot,CtrLit "role",CtrDot,CtrLit "paradox"],[CtrLit "cpu",CtrDot,CtrLit "0",CtrDot,CtrLit "user",CtrDot,CtrLit "role",CtrDot,CtrLit "paradox"]] $ expandParts [[CtrLit "cpu",CtrDot,CtrLit "0",CtrDot,CtrArray ["idle", "user"],CtrDot,CtrLit "role",CtrDot,CtrLit "paradox"]]

tests = $(testGroupGenerator)

main = defaultMain tests
