%% @author Roger
%% @doc @todo Add description to fs_app.


-module(fs_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
%% 
%% Also introduces log4erl. For more information see <a href="http://code.google.com/p/log4erl/" target="new">Google's Log4l Project</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
start(_Type, _StartArgs) ->
	application:start(log4erl),
	log4erl:conf("priv/log4erl.conf"),
	log4erl:info("Starting fs_app~n"),

	%% replace this with a supervisor
    case fs_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
    end.

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
	fs_server:stop(),
	application:stop(log4erl),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


