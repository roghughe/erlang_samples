%% @author Roger
%% @doc Test the sample List comprehension code

-include_lib("eunit/include/eunit.hrl").


-module(list_comp_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

ex1_test() ->
	?assertEqual([0,1,2,3,4,5],list_comp:ex1()).

ex2_test() ->
	?assertEqual([0.0,4.0,16.0],list_comp:ex2()).

ex3_test() ->
	?assertEqual([{area,20000},{area,3000}],list_comp:ex3()).


%% ====================================================================
%% Internal functions
%% ====================================================================


