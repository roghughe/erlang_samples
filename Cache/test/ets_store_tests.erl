%% @author Roger
%% @doc Tests for ets_store (which encapsulates ETS tables as part of its implementation)

-include_lib("eunit/include/eunit.hrl").

-module(ets_store_tests).

-define(KEY,key).
-define(VALUE,"value").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% @doc This is the test - where we define the fixture. The one we use here is:
%%  {setup, Setup, Cleanup, Instantiator}
%%
%% Take a look at: <a href="http://learnyousomeerlang.com/eunit">Learn You Some Erlang</a>
%%
%% Other fixtures inclde:
%% {setup, Setup, Instantiator}
%% {setup, Setup, Cleanup, Instantiator}
%% {setup, Where, Setup, Instantiator}
%% {setup, Where, Setup, Cleanup, Instantiator}
%%
insert_test_() ->
	{setup,			% tuple key
	 fun start/0,	% Test setup function
	 fun stop/1,	% teardown function 
	 fun insert/1}.	% The test function

%% @doc Called before the test
start() ->
	ets_store:init().

%% @doc Called after the test
stop(_) ->
	ets_store:stop().

%% The Test itself
insert(_)->
	[?_assert(ets_store:insert(?KEY, ?VALUE))].


lookup_test_() ->
	{setup,			% tuple key
	 fun start/0,	% Test setup function
	 fun stop/1,	% teardown function 
	 fun lookup/1}.	% The test function

lookup(_) ->
	ets_store:insert(?KEY, ?VALUE),
	[?_assertEqual({ok,?VALUE},ets_store:lookup(?KEY)),
	 ?_assertEqual({error, not_found},ets_store:lookup(wibble))].

delete_test() ->
	{setup,			% tuple key
	 fun start/0,	% Test setup function
	 fun stop/1,	% teardown function 
	 fun delete/1}.	% The test function


delete(_) ->
	?assert(ets_store:insert(?KEY, ?VALUE)),
	?assert(ets_store:delete(?KEY)),
	?assertEqual({error, not_found},simple_cache:lookup(?KEY)).


%% ====================================================================
%% Internal functions
%% ====================================================================


