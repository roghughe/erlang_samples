%%%-------------------------------------------------------------------
%%% @author Roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jun 2016 21:03
%%%-------------------------------------------------------------------
-module(rot_tests).
-author("Roger").

-include_lib("eunit/include/eunit.hrl").

-export([]).

rot_start_test() ->
  rot:rot_start().

rot_13_happy_flow_test() ->
  ?assertEqual("Ebtre",rot:rot("Roger", 13)).

rot_13_empty_list_test() ->
  ?assertEqual("",rot:rot("", 13)).
