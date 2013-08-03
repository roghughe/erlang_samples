%% @author Roger
%% @doc Tests for simple_cache_store - calls either ets_store or mnesia_store

-include_lib("eunit/include/eunit.hrl").

-module(simple_cache_store_tests_ignore_till_fixed).

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
	file:set_cwd("/tmp"),
	error_logger:info("CWD is: ~p",[file:get_cwd()]),
	simple_cache_store:init().

%% @doc Called after the test
stop(_) ->
	simple_cache_store:stop().

%% The Test itself
insert(_)->
	[?_assert(simple_cache_store:insert(?KEY, ?VALUE))].


lookup_test_() ->
	{setup,			% tuple key
	 fun start/0,	% Test setup function
	 fun stop/1,	% teardown function 
	 fun lookup/1}.	% The test function

lookup(_) ->
	simple_cache_store:insert(?KEY, ?VALUE),
	[?_assertEqual({ok,?VALUE},simple_cache_store:lookup(?KEY)),
	 ?_assertEqual({error, not_found},simple_cache_store:lookup(wibble))].

delete_test() ->
	{setup,			% tuple key
	 fun start/0,	% Test setup function
	 fun stop/1,	% teardown function 
	 fun delete/1}.	% The test function


delete(_) ->
	?assert(simple_cache_store:insert(?KEY, ?VALUE)),
	?assert(simple_cache_store:delete(?KEY)),
	?assertEqual({error, not_found},simple_cache_store:lookup(?KEY)).


%% ====================================================================
%% Internal functions
%% ====================================================================


