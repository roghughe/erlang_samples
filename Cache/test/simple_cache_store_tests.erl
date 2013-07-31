%% @author Roger
%% @doc Tests for simple_cache_store (which encapsulates ETS tables as part of its implementation)

-include_lib("eunit/include/eunit.hrl").

-module(simple_cache_store_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

insert_test_() ->
	{setup,			% tuple key
	 fun start/0,	% Test setup function
	 fun stop/1,	% teardown function 
	 fun insert/1}.	% The test function


start() ->
	simple_cache_store:init().

stop(_) ->
	simple_cache_store:stop().

insert(_)->
	[?_assert(simple_cache_store:insert(key, "value"))].


lookup_test_() ->
	{setup,			% tuple key
	 fun start/0,	% Test setup function
	 fun stop/1,	% teardown function 
	 fun lookup/1}.	% The test function


lookup(_) ->
	simple_cache_store:insert(key, "value"),
	[?_assertEqual({ok,"value"},simple_cache_store:lookup(key)),
	 ?_assertEqual({error, not_found},simple_cache_store:lookup(wibble))].



%% ====================================================================
%% Internal functions
%% ====================================================================


