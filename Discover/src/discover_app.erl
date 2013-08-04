-module(discover_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	application:start(log4erl),
	LogConfig = get_root_path() ++ "priv/log4erl.conf",
	error_logger:info_msg(LogConfig),
	log4erl:conf(LogConfig),
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

get_root_path() ->
	EnvParam = string:to_upper(erlang:atom_to_list('discover')),
	case os:getenv(EnvParam) of
  		false -> "./";
  		E -> E
	end.
