%% @author Roger
%% @doc @todo Add description to ets_sample_tests.

-include_lib("eunit/include/eunit.hrl").

-module(ets_sample_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

create_test() ->
	?assertEqual(ok,ets_sample:create()).



%% ====================================================================
%% Internal functions
%% ====================================================================


