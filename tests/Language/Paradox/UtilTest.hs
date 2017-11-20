{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Language.Paradox.UtilTest where

import Data.ByteString

import Test.Tasty
import Test.Tasty.TH

import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure)

import Language.Paradox.Grammar
import Language.Paradox.Util
import Text.Megaparsec



import Language.Paradox.GrammarCases

check_to_expr :: ByteString -> IO ()
check_to_expr exprT = isOk $ typeCheck gexpr
    where
        Right gexpr = runParser expr "" exprT
        isOk (Right _) = return ()
        isOk (Left e) = assertFailure ("Bad parse" ++ show e)

case_simple_infix_grammar :: Assertion
case_simple_infix_grammar                      = check_to_expr check_simple_infix_grammar
case_simple_ap_grammar :: Assertion
case_simple_ap_grammar                         = check_to_expr check_simple_ap_grammar
case_simple_infix_grammar_multi_space :: Assertion
case_simple_infix_grammar_multi_space          = check_to_expr check_simple_infix_grammar_multi_space
case_simple_ap_grammar_multi_space :: Assertion
case_simple_ap_grammar_multi_space             = check_to_expr check_simple_ap_grammar_multi_space
case_simple_grammar_spaces_around_parens :: Assertion
case_simple_grammar_spaces_around_parens       = check_to_expr check_simple_grammar_spaces_around_parens
case_infix_grammar_spaces_around_parens :: Assertion
case_infix_grammar_spaces_around_parens        = check_to_expr check_infix_grammar_spaces_around_parens
case_spaces_and_parens :: Assertion
case_spaces_and_parens                         = check_to_expr check_spaces_and_parens
case_space_at_end :: Assertion
case_space_at_end                              = check_to_expr check_space_at_end
case_space_at_start :: Assertion
case_space_at_start                            = check_to_expr check_space_at_start
case_dumb_counter_names_grammar :: Assertion
case_dumb_counter_names_grammar                = check_to_expr check_dumb_counter_names_grammar
case_scale_grammar_w_negative :: Assertion
case_scale_grammar_w_negative                  = check_to_expr check_scale_grammar_w_negative
case_arrity_2_infix_grammar :: Assertion
case_arrity_2_infix_grammar                    = check_to_expr check_arrity_2_infix_grammar
case_parens_grammar_into_infix :: Assertion
case_parens_grammar_into_infix                 = check_to_expr check_parens_grammar_into_infix
case_parens_grammar_with_ap :: Assertion
case_parens_grammar_with_ap                    = check_to_expr check_parens_grammar_with_ap
case_parens_grammar_with_ap_and_infix :: Assertion
case_parens_grammar_with_ap_and_infix          = check_to_expr check_parens_grammar_with_ap_and_infix
case_properties_grammar :: Assertion
case_properties_grammar                        = check_to_expr check_properties_grammar
case_properties_grammar_with_infix :: Assertion
case_properties_grammar_with_infix             = check_to_expr check_properties_grammar_with_infix
case_negative_value_in_properties :: Assertion
case_negative_value_in_properties              = check_to_expr check_negative_value_in_properties
case_simple_alias :: Assertion
case_simple_alias                              = check_to_expr check_simple_alias
case_simple_globs :: Assertion
case_simple_globs                              = check_to_expr check_simple_globs
case_partial_globs :: Assertion
case_partial_globs                             = check_to_expr check_partial_globs
case_partial_globs2 :: Assertion
case_partial_globs2                            = check_to_expr check_partial_globs2
case_simple_question_marks :: Assertion
case_simple_question_marks                     = check_to_expr check_simple_question_marks
case_globs_and_string_properties :: Assertion
case_globs_and_string_properties               = check_to_expr check_globs_and_string_properties
case_parens_and_string_properties :: Assertion
case_parens_and_string_properties              = check_to_expr check_parens_and_string_properties
case_extra_properties :: Assertion
case_extra_properties                          = check_to_expr check_extra_properties
case_heavy_nesting :: Assertion
case_heavy_nesting                             = check_to_expr check_heavy_nesting
case_offset_followed_by_nesting :: Assertion
case_offset_followed_by_nesting                = check_to_expr check_offset_followed_by_nesting
case_complex_offset_time_range :: Assertion
case_complex_offset_time_range                 = check_to_expr check_complex_offset_time_range
case_constant_line :: Assertion
case_constant_line                             = check_to_expr check_constant_line
case_constant_line_and_cons :: Assertion
case_constant_line_and_cons                    = check_to_expr check_constant_line_and_cons
case_constant_line_and_cons_w_parens :: Assertion
case_constant_line_and_cons_w_parens           = check_to_expr check_constant_line_and_cons_w_parens
case_negative_scale :: Assertion
case_negative_scale                            = check_to_expr check_negative_scale
case_shell_escaping :: Assertion
case_shell_escaping                            = check_to_expr check_shell_escaping
case_globbing_and_aliasing :: Assertion
case_globbing_and_aliasing                     = check_to_expr check_globbing_and_aliasing
case_globbing_and_aliasing_with_infix_operator :: Assertion
case_globbing_and_aliasing_with_infix_operator = check_to_expr check_globbing_and_aliasing_with_infix_operator
case_aliasing :: Assertion
case_aliasing                                  = check_to_expr check_aliasing
case_aliasing_with_infix_operator :: Assertion
case_aliasing_with_infix_operator              = check_to_expr check_aliasing_with_infix_operator
case_grammarWebResponse :: Assertion
case_grammarWebResponse                        = check_to_expr check_grammarWebResponse
case_grammarWebResponseA :: Assertion
case_grammarWebResponseA                       = check_to_expr check_grammarWebResponseA
case_grammar_conditional_constraints :: Assertion
case_grammar_conditional_constraints           = check_to_expr check_grammar_conditional_constraints
case_uppercase :: Assertion
case_uppercase                                  = check_to_expr check_uppercase

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
