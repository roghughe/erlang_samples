%%%-------------------------------------------------------------------
%%% @author Roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jun 2016 22:41
%%%-------------------------------------------------------------------

-module(if_example_tests).
-author("Roger").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).

compare_numbers_not_equal_test() ->
  ?assertEqual(nomatch,if_example:compare_numbers(2,3)).

compare_numbers_alt_but_better_not_equal_test() ->
  ?assertEqual(nomatch,if_example:compare_numbers_alt_but_better(2,3)).

compare_numbers_equal_test() ->
  ?assertEqual(match,if_example:compare_numbers(3,3)).

compare_once_more_test() ->
  ?assertEqual(equal,if_example:compare_once_more(3,3)).

compare_once_more_gt_test() ->
  ?assertEqual(greaterThan,if_example:compare_once_more(5,3)).

compare_once_more_lt_test() ->
  ?assertEqual(lessThan,if_example:compare_once_more(5,8)).

compare_chars_test() ->
  ?assertEqual(a,if_example:compare_chars('A')).

%% This should  print the time
valid_time_pass_test() ->
  if_example:valid_time({{2016,12,24},{12,12,12}}).

%% This should  print the error message
valid_time_fail_test() ->
  if_example:valid_time("heelloooo").

%% This should print the time - with the guard
valid_time_guard_pass_test() ->
  if_example:valid_time_guard({3000,33,3434}).

%% This should  print the error message
valid_time__guard_fail_test() ->
  if_example:valid_time("heelloooo").

