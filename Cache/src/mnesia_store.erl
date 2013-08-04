%% @author Roger
%% @doc Simple wrapper around Mnesia tables that maps a key to a Pid
%% The table is accessed via its table name (?TABLE_ID) rather than the 
%% value returned by ets:new(...)


-module(mnesia_store).

-define(WAIT_TIMEOUT,5000).

%% ====================================================================
%% API functions
%% ====================================================================
-export([lookup/1, delete/1, insert/2, init/0,stop/0]).

-record(key_to_pid, {key,pid}).

%% Init the module. As a matter of style init(..) and / or start_XXX() methods go at the top of a module
init()->
	log4erl:info("Start the Mnesia store - clustered version "),
	reset_db(),
	{ok,CacheNodes} = discover:fetch_resources(simple_cache),
	{RetVal,Status} = dynamic_db_init(lists:delete(node(),CacheNodes)),
	
	case RetVal of 
		atomic ->
			ok;	
		_ ->
			log4erl:error("Cannot create Mnesia table: ~p",[Status]),
			{RetVal,Status}
	end.

insert(Key,Pid) ->
	case mnesia:dirty_write(#key_to_pid{key=Key,pid=Pid}) of 
		ok -> 
			true;
		_ ->
			false
	end.

lookup(Key) ->
	case mnesia:dirty_read(key_to_pid, Key) of
		[{key_to_pid,Key,Pid}] -> 
			case is_pid_alive(Pid) of
				true -> {ok,Pid};
				false -> {error,not_found}
			end;
		[]          -> {error, not_found}
	end.

delete(Pid) ->
	case mnesia:dirty_index_read(key_to_pid,Pid,#key_to_pid.pid) of
		[#key_to_pid{} = Record] ->
			mnesia:dirty_delete_object(Record);
		[] ->
			ok
	end.

stop() ->
	%mnesia:stop().
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

is_pid_alive(Pid) when node(Pid) =:= node() ->
	log4erl:info("Checking Pid alive:: ~p ",[Pid]),
	is_process_alive(Pid);
is_pid_alive(Pid) ->
	lists:member(node(Pid),nodes()) andalso
		(rpc:call(node(Pid),erlang,is_process_alive,[Pid]) =:= true).

reset_db() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:start().


dynamic_db_init([]) ->
	mnesia:create_table(key_to_pid,
						[{index,[pid]},
						 {attributes,record_info(fields,key_to_pid)}
						]);
dynamic_db_init(CacheNodes) ->
	add_extra_nodes(CacheNodes).


add_extra_nodes([Node | T]) ->
	case mnesia:change_config(extra_db_nodes, [Node]) of 
		{ok,[Node]} ->
			mnesia:add_table_copy(schema,node(),ram_copies),	
			mnesia:add_table_copy(key_to_pid, node(), ram_copies),
			Tables = mnesia:system_info(tables),
			RetVal = mnesia:wait_for_tables(Tables, ?WAIT_TIMEOUT),
			log4erl:info("~p = mnesia:wait_for_tables(~p, ?WAIT_TIMEOUT)",[RetVal,Tables]),
			case RetVal of
				ok ->
					{atomic,ok};
				_ ->
					RetVal
			end;
		{error, Reason}  ->
			log4erl:info("add_extra_nodes - mnesia:change {error, ~p}",[Reason]),
			add_extra_nodes(T)
	end.
