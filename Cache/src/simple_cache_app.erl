-module(simple_cache_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	case simple_cache_sup:start_link() of
		{ok, Pid} -> 
			log4erl:info("App Start Okay"),
			{ok,Pid};
		Other ->
			log4erl:info("App start Error: ~p",[Other]),
			{error,Other}
	end.

stop(_State) ->
    ok.
