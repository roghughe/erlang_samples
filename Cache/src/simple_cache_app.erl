-module(simple_cache_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(StartType, StartArgs) ->
	log4erl:conf("priv/log4erl.conf"),
	simple_cache_store:init(),
	log4erl:info("Start the Cache App -> Type: ~p  StartArgs: ~p",[StartType, StartArgs]),
	case simple_cache_sup:start_link(StartArgs) of
		{ok, Pid} -> 
			log4erl:info("App Start Okay"),
			simple_cache_event:add_handler(simple_cache_event_handler, []),
			{ok,Pid};
		Other ->
			log4erl:info("App start Error: ~p",[Other]),
			{error,Other}
	end.

stop(_State) ->
	simple_cache_store:stop(),
    ok.
