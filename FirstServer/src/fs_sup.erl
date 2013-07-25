%% @author Roger
%% @doc @todo Add description to fs_sup.


-module(fs_sup).
-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER,?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

%% @doc This is a API entry point that starts the supervisor
start_link() ->
	log4erl:info("Starting the supervisor"),
	supervisor:start_link({local,?SERVER}, ?MODULE, []).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
%% This is the callback function. Somewhere in the <tt>supervisor:start_link(..)</tt> function, there is a callback that comes out here,
%% so that the supervisor can start its worker process(es) or additional supervisor(s).
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
	log4erl:info("supervisor - init callback"),
	
	Server = {fs_server, {fs_server,start_link,[]},
			  permanent,2000,worker,[fs_server]},
	Children = [Server],
	RestartStrategy = {one_for_one,0,1},
	{ok, {RestartStrategy,Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


