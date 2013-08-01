%% @author Roger
%% @doc Demonstres the return tuple from an supervisor init/1 callback

-include_lib("eunit/include/eunit.hrl").


-module(simple_cache_element_sup_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% Demonstrates the return tuple / required for a supervisor init method 
init_test() ->
    Result = simple_cache_element_sup:init([]),
	Children = [{simple_cache_element, {simple_cache_element,start_link,[]},
			   temporary,brutal_kill,worker,[simple_cache_element]}],
	RestartStrategy = {simple_one_for_one,0,1},
	?assertEqual({ok,{RestartStrategy,Children}},Result).


%% ====================================================================
%% Internal functions
%% ====================================================================


