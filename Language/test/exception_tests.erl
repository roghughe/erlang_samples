%% @author Roger
%% @doc Tests the sample exceptions code

-include_lib("eunit/include/eunit.hrl").

-module(exception_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

catch_all_test() ->
	?assertEqual(ok,exception:catch_all()).
	

%% Test catching different types of exceptions
%% Return the results from each exception as a list - terminated by [] to make it a proper list
catch_me_test_() ->
	[?_assertEqual(got_throw_oops,exception:catch_me(oops)),
	 ?_assertEqual({got_throw, "The Reason"},exception:catch_me(throw)),
	 ?_assertEqual({got_exit, "The Reason"},exception:catch_me(exit)),
	 ?_assertEqual(error,exception:catch_me(error))].


tryof_test() ->
	?assertEqual(ok,exception:tryof(pass)).

after_clause_test() ->
	?assertEqual(ok,exception:after_clause()).

%% This returns a stacj trace, but that might change depending upon install location
stack_test() ->
	_RetVal = exception:skack(oops).


%% ====================================================================
%% Internal functions
%% ====================================================================


