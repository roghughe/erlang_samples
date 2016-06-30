%%%-------------------------------------------------------------------
%%% @author Roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jun 2016 13:25
%%%-------------------------------------------------------------------
-module(types_tests).
-author("Roger").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).


conversion_examples_test() ->

  Result = types:conversion_examples(),
  ?assertEqual({"54",54.5,<<"54">>,54,{53,52}},Result).

guard_examples_test() ->
  Result = types:guard_examples(),
  ?assertEqual(ok,Result).