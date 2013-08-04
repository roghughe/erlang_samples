-module(simple_cache_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(StartType, StartArgs) ->
	log4erl:conf(config:get_root_path() ++ "priv/log4erl.conf"),
	log4erl:info("Start the Cache App -> Type: ~p  StartArgs: ~p",[StartType, StartArgs]),
	ok = ensure_contact(),
	discover_resources(),
	simple_cache_store:init(),
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



%% ====================================================================
%% Internal functions
%% ====================================================================

ensure_contact() ->
	{_Status,DefaultNodes} = config:get(contact_nodes),
	log4erl:info("DefaultNodes: ~p",[DefaultNodes]),
	case get_env(simple_cache,contact_nodes,DefaultNodes) of
		[] ->
			{error,no_contact_nodes};
		ContactNodes ->
			log4erl:info("ContactNodes: ~p",[ContactNodes]),
			ensure_contact(ContactNodes)
	end.

get_env(AppName,Key,Default) ->
	case application:get_env(AppName,Key) of 
		undefined -> Default;
		{ok,Value} -> Value
	end.

ensure_contact(ContactNodes) ->
	Answering = [N || N <- ContactNodes,net_adm:ping(N) =:= pong],
	case Answering of
		[] ->
			{error,no_contact_nodes_reachable};
		_ ->
			log4erl:info("Answering Nodes: ~p",[Answering]),
			DefaultTime = 6000,
			WaitTime = get_env(simple_cache,wait_time,DefaultTime),
			wait_for_nodes(length(Answering),WaitTime)
	end.

wait_for_nodes(MinNodes,WaitTime) ->
	Slices = 10,
	SliceTime = round(WaitTime/Slices),
	log4erl:info("wait_for_nodes(~p,~p) SliceTime: ~p",[MinNodes,WaitTime,SliceTime]),

	wait_for_nodes(MinNodes,SliceTime,Slices).

wait_for_nodes(MinNodes,SliceTime,0) ->
	log4erl:info("wait_for_nodes(~p,~p,0)",[MinNodes,SliceTime]),

	ok;
wait_for_nodes(MinNodes,SliceTime,Iterations) ->
	log4erl:info("wait_for_nodes(~p,~p,~p)",[MinNodes,SliceTime,Iterations]),
	case length(nodes()) > MinNodes of
		true ->
			log4erl:info("wait_for_nodes in true - returning ok"),
			ok;
		false ->
			timer:sleep(SliceTime),
			wait_for_nodes(MinNodes,SliceTime,Iterations-1)
	end.

%% This uses the discovery service to figure out which other instances are running
discover_resources() ->
	discover:add_local_resource(simple_cache, node()),
	discover:add_target_resource_type(simple_cache),
	discover:trade_resources(),
	timer:sleep(2500).

