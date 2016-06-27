%% @author Roger
%% @doc Test the hello module. Example of a eunit test module

-module(hello_tests).

-include_lib("eunit/include/eunit.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% My first test. hello_world() prints "Hello World!" and will return ok (the default).
hello_world_test() ->
	?assertEqual(ok,hello:hello_world()).


%% Example of a single test 
greet_test() ->
	%%?assertEqual(ok,hello:greet("Roger")).
	hello:greet("Roger").

%% single Test get one value from the tuple and returns it.
height_from_point_test() ->
	?assertEqual(50,hello:height_from_point({rectangle,50,100})).

%% This is a list of tests that uses a 'test generator'. It counts as a single test, but is used to test a whole bunch of stuff.
height_from_point_test_() ->
	[?_assertEqual(50,hello:height_from_point({rectangle,50,100})),
	 ?_assertEqual(0,hello:height_from_point({rectangle,0,100}))].


callsFunction_test() ->
	?assertEqual(ok,hello:callsFunction("Roger")).

eitherOrBoth_test_() ->
	[?_assert(hello:eitherOrBoth(true, true)),
	 ?_assert(hello:eitherOrBoth(true, false)),
	 ?_assert(hello:eitherOrBoth(false, true)),
	 ?_assertNot(hello:eitherOrBoth(false, false))].

	
%% ====================================================================
%% Internal functions
%% ====================================================================


