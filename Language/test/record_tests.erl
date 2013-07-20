%% @author Roger
%% @doc @todo Add description to record_test.

-include_lib("eunit/include/eunit.hrl").

-module(record_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

create_test() ->
	Expected = {customer,"Roger Hughes","info@captaindebug.com","555 1234"},
	?assertEqual(Expected,record:create()).


access_test() ->
	?assertEqual({"Roger Hughes", "info@captaindebug.com", "555 1234"},record:access()).

update_test() ->
	?assertEqual({customer,"Roger Hughes","info@captaindebug.com","555 0987"},record:update()).

update2_test() ->
	?assertEqual({"Roger Hughes","555 1234"},record:update2()).

update3_test() ->
	?assertEqual({"Roger Hughes",undefined},record:update3()).


update4_test() ->
	?assertEqual(missing,record:update4()).

access_ext_test() ->
	?assertEqual({"23 Railway Cuttings","East Cheam"},record:access_ext()).



%% ====================================================================
%% Internal functions
%% ====================================================================


