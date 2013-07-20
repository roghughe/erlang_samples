%% @author Roger
%% @doc @todo Add description to bitstring_test.

-include_lib("eunit/include/eunit.hrl").

-module(bitstring_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

ex1_test() ->
	?assertEqual(<<0,254,255,0,1>>,bitstring:ex1()).

ex2_test() ->
	?assertEqual(<<"Roger">>,bitstring:ex2()).

ex3_test() ->
	?assertEqual(<<"Roger">>,bitstring:ex3()).

ex4_test() ->
	?assertEqual(<<1,2,3,4>>,bitstring:ex4()).

ex5_test() ->
	{Version,Size} = bitstring:ex5(),
	[?_assertEqual(2,Version),
	 ?_assertEqual(0,Size)].

ex6_test() ->
	?assertEqual(<<41,203,23:5>>,bitstring:ex6()).

ex7_test() ->
	?assertEqual(<<1,2,3,4,5,6,7>>,bitstring:ex7()).

ex8_test() ->
	?assertEqual( [1,2,3,4,5,6,7],bitstring:ex8()).


%% ====================================================================
%% Internal functions
%% ====================================================================


