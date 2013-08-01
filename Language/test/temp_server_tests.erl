%% @author Roger
%% @doc Tests for the kamikazi server

-include_lib("eunit/include/eunit.hrl").


-module(temp_server_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% @doc Demonstrates the output of a standard error message 
start_link_test() ->
	Pid = spawn(fun temp_server:start_link/0),
	?assert(is_pid(Pid)).




%% ====================================================================
%% Internal functions
%% ====================================================================


