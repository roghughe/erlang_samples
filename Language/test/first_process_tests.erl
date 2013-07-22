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
	
%% If this fails it returns a message in a tuple rather than a reference in the form #Ref<1.2.3.4>
monitor_you_test() ->
	?assert(is_reference(first_process:monitor_you())).

registered_test() ->
	?assert(is_list(first_process:my_registered())).

is_init_process_running_test() ->
	?assert(first_process:is_init_process_running()).

is_init_process_running2_test() ->
	?assert(first_process:is_init_process_running2()).

my_register_test() ->
	?assert(first_process:my_register()).

%% ====================================================================
%% Internal functions
%% ====================================================================


