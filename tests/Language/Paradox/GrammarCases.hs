{-# LANGUAGE OverloadedStrings #-}
module Language.Paradox.GrammarCases where

import Data.ByteString (ByteString)

check_simple_infix_grammar :: ByteString
check_simple_infix_grammar =
    "tep.tep,a.counter,a.200\
    \ | sum"

check_simple_ap_grammar :: ByteString
check_simple_ap_grammar =
    "sum tep.tep,a.counter,a.200"

check_simple_infix_grammar_multi_space :: ByteString
check_simple_infix_grammar_multi_space =
    "tep.tep,a.counter,a.200  | sum |   alias   \"asdasd\""

check_simple_ap_grammar_multi_space :: ByteString
check_simple_ap_grammar_multi_space =
    "alias \"asdasd\" (sum  tep.tep,a.counter,a.200)"

check_simple_grammar_spaces_around_parens :: ByteString
check_simple_grammar_spaces_around_parens =
    "alias \"asdasd\" ( sum  tep.tep,a.counter,a.200  )"

check_infix_grammar_spaces_around_parens :: ByteString
check_infix_grammar_spaces_around_parens =
    "( sum  tep.tep,a.counter,a.200  ) | alias \"asdasd\""

check_spaces_and_parens :: ByteString
check_spaces_and_parens =
    "( sum  tep.tep,a.counter,a.200 { offset = -100 } ) | scale -200.0 > \"asdasd\""

check_space_at_end :: ByteString
check_space_at_end =
    "tep.tep,a.counter,a.200 | alias \"asdasd\"     "

check_space_at_start :: ByteString
check_space_at_start =
    "     tep.tep,a.counter,a.200 | alias \"asdasd\""

check_dumb_counter_names_grammar :: ByteString
check_dumb_counter_names_grammar =
    "sum !t>ep.>tep,_<a.-counter+another,a.!2-<00"

check_scale_grammar_w_negative :: ByteString
check_scale_grammar_w_negative =
    "tep.counter\
    \ | scale -200.0"

check_arrity_2_infix_grammar :: ByteString
check_arrity_2_infix_grammar =
    "tep.tep,a.counter,a.200\
    \ | sub tep.total"

check_parens_grammar_into_infix :: ByteString
check_parens_grammar_into_infix =
    "(sum tep.tep,a.counter,a.200)\
    \ | sub tep.total"

check_parens_grammar_with_ap :: ByteString
check_parens_grammar_with_ap = "sub tep.total (sum tep.tep,a.counter,a.200)"

check_parens_grammar_with_ap_and_infix :: ByteString
check_parens_grammar_with_ap_and_infix = "sub tep.total (tep.tep,a.counter,a.200 | sum)"


check_properties_grammar :: ByteString
check_properties_grammar = "tep.tep { offset = \"-7D\" }\
                           \ | sum"

check_properties_grammar_with_infix :: ByteString
check_properties_grammar_with_infix = "tep.tep,a.counter,a.200 { offset = 100 }\
                                      \ | sub tep.total"

check_negative_value_in_properties :: ByteString
check_negative_value_in_properties = "tep.tep,a.counter,a.200 { offset = -100 }\
                                     \ | sub tep.total"

check_simple_alias :: ByteString
check_simple_alias =
    "tep.tep,a.counter,a.200\
    \ | graphCount"

check_simple_globs :: ByteString
check_simple_globs = "users.*"

check_partial_globs :: ByteString
check_partial_globs = "users.ab*ab"

check_partial_globs2 :: ByteString
check_partial_globs2 = "users.*ab*bc?d"

check_simple_question_marks :: ByteString
check_simple_question_marks = "users.?abc"

check_array_counter_notation :: ByteString
check_array_counter_notation = "users.[die,online,red]"


check_globs_and_string_properties :: ByteString
check_globs_and_string_properties = "users.*\
                                    \ | sub (users.* { offset = \"-1W\" } | sum)"

check_parens_and_string_properties :: ByteString
check_parens_and_string_properties = "users.mobile,users.3d\
                                     \ | sub (users.mobile,users.3d { offset = \"-1W\" } | sum)"

check_extra_properties :: ByteString
check_extra_properties = "users.mobile,users.3d\
                         \ | sub (users.mobile,users.3d { offset = \"-1W\", dropEmpty = 1 } | sum)"

check_heavy_nesting :: ByteString
check_heavy_nesting = "users.mobile,users.3d\
                      \ | sum\
                      \ | sub (users.mobile,users.3d { offset = \"-1W\" } | sum)"

--This means to query users.mobile and users.3d 2 times add the current
--time range and then subtract that from the last week offsets time ranges
--sum
--Once for current time range
--Once for 1w * 7d * 24h * 60m * 60s offset in the past


check_offset_followed_by_nesting :: ByteString
check_offset_followed_by_nesting = "users.mobile,users.3d { offset = \"-1W\" }\
                                   \ | sum\
                                   \ | sub (users.mobile,users.3d | sum)"

check_complex_offset_time_range :: ByteString
check_complex_offset_time_range = "users.mobile,users.3d\
                                  \ | sum\
                                  \ | sub (users.mobile,users.3d { offset = \"-1Y1M1W1D1h1m1s\" } | sum)"

check_constant_line :: ByteString
check_constant_line = "constantLine 5.5"

check_constant_line_and_cons :: ByteString
check_constant_line_and_cons = "constantLine 5.0\
                               \ | cons asdb.imvu.com"

check_constant_line_and_cons_w_parens :: ByteString
check_constant_line_and_cons_w_parens = "asdb.imvu.com\
                                        \ | cons (constantLine 5.0)"

check_negative_scale :: ByteString
check_negative_scale = "asdb.imvu.com\
                       \ | scale -1.0\
                       \ | offset 100.0"

check_shell_escaping :: ByteString
check_shell_escaping = "asdb.imvu.com.host.{'gethost -r gateway'}\
                       \ | scale -1.0\
                       \ | offset 100.0"


check_globbing_and_aliasing :: ByteString
check_globbing_and_aliasing = "asdb.imvu.com.host.* { offset = \"-1W\" }\
                              \ | sum\
                              \ | alias \"A Name\""

check_globbing_and_aliasing_with_infix_operator :: ByteString
check_globbing_and_aliasing_with_infix_operator = "asdb.imvu.com.host.* { offset = \"-1W\" }\
                                                  \ | sum\
                                                  \ > \"A Name\""

check_uppercase :: ByteString
check_uppercase = "asdb.imvu.com.host.HOST12340"

check_aliasing :: ByteString
check_aliasing = "asdb.imvu.com.host\
                 \ | alias \"A Name\""

check_aliasing_with_infix_operator :: ByteString
check_aliasing_with_infix_operator = "asdb.imvu.com.host\
                                     \ > \"A Name\""

check_grammarWebResponse :: ByteString
check_grammarWebResponse =
   "apache.response-time.95th.role.web\
   \ | removeAboveValue 60.0\
   \ | alias \"Response Time: web\"\
   \ | graphOptions \"{\\\"color\\\": \\\"#00B34D\\\", \\\"strokeWidth\\\":3.0}\""

check_grammarWebResponseA :: ByteString
check_grammarWebResponseA =
    "apache.response-time.95th.role.web\
    \ | removeAboveValue 60.0\
    \ | graphOptions \"{\\\"color\\\": \\\"#00B34D\\\", \\\"strokeWidth\\\":3.0}\"\
    \ > \"Response Time: web\""

check_grammar_conditional_constraints :: ByteString
check_grammar_conditional_constraints =
    "foo.web.response.5xx.role.foo-bar-api\
    \ | whenElse (hasAnyValue greaterThan getAvg 2.5)\
               \ (dyFillAlpha 0.9 . dyFillGraph True\
                                \ . dyColor \"darkred\"\
                                \ . applyWithValue dyConstrainMax (getLargestInRange getAvg 10.0 . mulD 1.1 . largerD 2.5)\
               \)\
               \ (dyConstrain 0.0 2.5)"
