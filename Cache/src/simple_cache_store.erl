%% @author Roger
%% @doc Simple wrapper around ETS tables that maps a key to a Pid
%% The table is accessed via its table name (?TABLE_ID) rather than the 
%% value returned by ets:new(...)


-module(simple_cache_store).

%% ====================================================================
%% API functions
%% ====================================================================
-export([lookup/1, delete/1, insert/2, init/0,stop/0]).

-define(TABLE_ID,?MODULE).

%% Init the module. As a matter of style init(..) and / or start_XXX() methods go at the top of a module
init()->
	log4erl:info("Start the store"),
	
	% 'public' marks the table as available to many / any processes.
	% 'named_table' makes it easily available
	ets:new(?TABLE_ID,[public,named_table]),
	ok.

insert(Key,Pid) ->
	ets:insert(?TABLE_ID, {Key,Pid}).

lookup(Key) ->
	case ets:lookup(?TABLE_ID, Key) of
		[{Key,Pid}] -> {ok,Pid};
		[]          -> {error, not_found}
	end.

delete(Pid) ->
	ets:match_delete(?TABLE_ID,{'_',Pid}).

stop() ->
	ets:delete(?TABLE_ID).

%% ====================================================================
%% Internal functions
%% ====================================================================


