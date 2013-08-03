%% @author Roger
%% @doc @todo Add description to simple_cache+sup.


-module(simple_cache_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

-define(SERVER,?MODULE).

%% Args - The startup list. Could be the tables type used 
start_link(Args) ->
	log4erl:info("simple_cache_sup:start_link(~p)",[Args]),
	supervisor:start_link({local,?SERVER},?MODULE,Args).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
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
init(Args) ->
	log4erl:info("simple_cache_sup:init(~p)",[Args]),
	
	ElementSup = {simple_cache_element_sup, {simple_cache_element_sup,start_link,[Args]},
					permanent,2000,supervisor,[simple_cache_element]},
	
	EventManager = {simple_cache_event, {simple_cache_event,start_link,[]},
					permanent,2000,worker,[simple_cache_event]},
	Children = [ElementSup,EventManager],
	RestartStrategy = {one_for_one,4,3600},
	{ok,{RestartStrategy,Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


