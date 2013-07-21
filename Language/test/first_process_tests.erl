%% @author Roger
%% @doc Simply run the ping-pong process

-include_lib("eunit/include/eunit.hrl").

-module(first_process_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

run_test() ->
	?assertEqual(ok,first_process:run()).

print_test() ->
	?assertEqual(sent,first_process:out()).

print2_test() ->
	?assertEqual(sent,first_process:out2()).

hello_process_test() ->
	?assertEqual(sent,first_process:hello_process()).

do_link1_test() ->
	spawn(first_process,do_link1,[]).

do_link2_test() ->
	?assertEqual(ok,first_process:do_link2()).
	
	

%% ====================================================================
%% Internal functions
%% ====================================================================


