-module(discover_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	application:start(log4erl),
	log4erl:conf("priv/log4erl.conf"),
	log4erl:info("Starting discover_app~n"),

    case discover_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
    end.

stop(_State) ->
	discover_sup:stop(),
	application:stop(log4erl),
    ok.
