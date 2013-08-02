
-module(discover_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc This is a API entry point that starts the supervisor
start_link() ->
	log4erl:info("Starting the supervisor"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

	Child = ?CHILD(discover_srv,worker),
	RestartStrategy = {one_for_one,0,1},

	log4erl:info("discover_sup:init([]) - Child: ~p  RestartStrategy: ~p",[Child,RestartStrategy]),
	Children = [Child],
    {ok, {RestartStrategy, Children} }.


