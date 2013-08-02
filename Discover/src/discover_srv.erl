%% @author Roger
%% @doc @todo Add description to discover_srv.


-module(discover_srv).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER,?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
		 add_target_resource_type/1,
		 add_local_resource/2,
		 fetch_resources/1,
		 trade_resources/0]).


start_link() ->
	log4erl:info("srv start_link"),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


add_target_resource_type(Type) ->
	log4erl:info("add_target_resource_type(~p)",[Type]),
	gen_server:cast(?SERVER,{add_target_resource,Type}).

add_local_resource(Type,Instance) ->
	log4erl:info("add_local_resource(~p,~p)",[Type,Instance]),
	gen_server:cast(?SERVER, {add_local_resource,{Type,Instance}}).

fetch_resources(Type)->
	log4erl:info("fetch_resources(~p)",[Type]),
	gen_server:call(?SERVER,{fetch_resources,Type}).

trade_resources() ->
	log4erl:info("trade_resources() called"),
	gen_server:cast(?SERVER,trade_resources).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {target_resource_types,		%% I want...
				local_resource_tuples,		%% I have
				found_resource_tuples		%% What I've found
				}).



%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	log4erl:info("srv init([])"),
    {ok, #state{target_resource_types = [],
				local_resource_tuples = dict:new(),
				found_resource_tuples = dict:new()
				}
	}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({fetch_resources,Type}, _From, State) ->
	log4erl:info("handle_call({fetch_resources,~p},~p,~p)",[Type,_From,State]),
    Reply = dict:find(Type,State#state.found_resource_tuples),
	log4erl:info("RetVal=~p",[Reply]),
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
	log4erl:info("handle_call() -- unwanted call"),
	{reply,ok,State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({add_target_resource,Type},State) ->
	log4erl:info("handle_cast({add_target_resource,~p},~p)",[Type,State]),
	TargetTypes = State#state.target_resource_types,
	NewTargetTypes = [Type | lists:delete(Type,TargetTypes)],
	RetVal= {noreply, State#state{target_resource_types = NewTargetTypes}},
	log4erl:info("RetVal=~p",[RetVal]),
	RetVal;
handle_cast({add_local_resource,{Type,Instance}},State) ->
	log4erl:info("handle_cast({add_local_resource,{~p,~p}},~p)",[Type,Instance,State]),
	ResourceTuples = State#state.local_resource_tuples,
	NewResourceTuples = add_resource(Type,Instance,ResourceTuples),
	RetVal={noreply,State#state{local_resource_tuples = NewResourceTuples}},
	log4erl:info("RetVal=~p",[RetVal]),
	RetVal;
handle_cast(trade_resources,State) ->
	log4erl:info("handle_cast(trade_resources,~p)",[State]),
	ResourceTuples = State#state.local_resource_tuples,
	AllNodes = [node() | nodes()],
	log4erl:info("AllNodes,~p~nResourceTuples: ~p",[AllNodes,ResourceTuples]),
	lists:foreach(fun(Node) ->
						log4erl:info("Sorting Node,~p)",[Node]),
						gen_server:cast({?SERVER,Node},
									   {trade_resources,{node(),ResourceTuples}})
				 end,
				 AllNodes),
	RetVal={noreply,State},
	log4erl:info("RetVal=~p",[RetVal]),
	RetVal;
handle_cast({trade_resources, {ReplyTo,Remotes}},
			#state{local_resource_tuples = Locals,
				   target_resource_types = TargetTypes,
				   found_resource_tuples = OldFound} = State) ->
	log4erl:info("handle_cast({trade_resources,{~p,~p}},~p)",[ReplyTo,Remotes,State]),

	FilteredRemotes = resources_for_types(TargetTypes,Remotes),
	NewFound = add_resources(FilteredRemotes,OldFound),
	case ReplyTo of
		noreply ->
			ok;
		_		->
			gen_server:cast({?SERVER,ReplyTo},{trade_resources, {noreply,Locals}})
	end,
	RetVal = {noreply, State#state{found_resource_tuples = NewFound}},
	log4erl:info("RetVal=~p",[RetVal]),
	RetVal;
handle_cast(_AnyOtherMessage, State) ->
	log4erl:info("handle_cast() -- Unknown cast call"),
    RetVal = {noreply, State},
	log4erl:info("RetVal=~p",[RetVal]),
	RetVal.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

add_resource(Type,Resource,ResourceTuples) ->
	case dict:find(Type,ResourceTuples) of 
		{ok,ResourceList} ->
			NewList = [Resource | lists:delete(Resource, ResourceList)],
			dict:store(Type,NewList,ResourceTuples);
		error->
			dict:store(Type,[Resource],ResourceTuples)
	end.

add_resources([{Type,Resource} | T],ResourceTuples) ->
	add_resources(T,add_resource(Type,Resource,ResourceTuples));
add_resources([],ResourceTuples) ->
	ResourceTuples.

resources_for_types(Types,ResourceTuples) ->
	Fun = fun(Type,Acc) ->
		  	case dict:find(Type,ResourceTuples) of
				{ok,List} ->
					[{Type,Instance} || Instance <- List] ++ Acc;
				error ->
					Acc
			end
		end,
	lists:foldl(Fun,[],Types).


