
-module(simple_cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(SERVER,?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	log4erl:info("Starting the supervisor"),
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Called by the simple_cache_element to start a child element cache process
start_child(Value,LeaseTime) ->
	log4erl:info("simple_cache_sup:start_child(~p,~p)",[Value,LeaseTime]),
	supervisor:start_child(?SERVER, [Value,LeaseTime]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	log4erl:info("simple_cache_sup:init([])"),
	Element = {simple_cache_element, {simple_cache_element,start_link,[]},
			   temporary,brutal_kill,worker,[simple_cache_element]},
	Children = [Element],
	RestartStrategy = {simple_one_for_one,0,1},
    {ok, {RestartStrategy, Children} }.

