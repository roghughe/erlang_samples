%% @author Roger
%% @doc Sum tests the the first server (fs)

-include_lib("eunit/include/eunit.hrl").

-module(fs_app_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

start_test() -> 

	fs_app:start(foo, bar),
	
	Count = fs_server:get_count(),

	fs_app:stop(foo),
	?assertEqual(0,Count).




%% ====================================================================
%% Internal functions
%% ====================================================================


