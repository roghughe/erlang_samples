%% @author Roger
%% @doc Server layer that chooses the table type to use depending upon the environment 
%% configured in the simple_cache.app.src file - theres's no need to reconfigure this on the fly.
%%
%% There isn't really any practical use for this, except that it keeps the ETS sample code in the app, 
%% giving a good comparison between that and mnesia tables


-module(simple_cache_store).

%% ====================================================================
%% API functions
%% ====================================================================
-export([lookup/1, delete/1, insert/2, init/0,stop/0]).

-define(TABLE_ID,?MODULE).

%% Init the module. As a matter of style init(..) and / or start_XXX() methods go at the top of a module
init()->
	log4erl:info("Start the store"),
	{Status,Type} = config:get(table_type),
	log4erl:info("Table Type ~p",[Type]),
	case {Status,Type} of 
		{ok,ets} ->
			ets_store:init(),
			RetVal = ok;
		{ok,mnesia} ->
			RetVal = mnesia_store:init();
		{error, Reason} ->
			RetVal = {error, Reason}
	end,
	RetVal.

insert(Key,Pid) ->
	log4erl:info("insert(~p,~p)",[Key,Pid]),
	case config:get(table_type) of 
		{ok,ets} ->
			RetVal = ets_store:insert(Key,Pid);
		{ok,mnesia} ->
			RetVal = mnesia_store:insert(Key,Pid);
		{error, Reason} ->
			RetVal = {error, Reason}
	end,
	RetVal.

lookup(Key) ->
	log4erl:info("lookup(~p)",[Key]),
	case config:get(table_type) of 
		{ok,ets} ->
			RetVal = ets_store:lookup(Key);
		{ok,mnesia} ->
			RetVal = mnesia_store:lookup(Key);
		{error, Reason} ->
			RetVal = {error, Reason}
	end,
	RetVal.
	

delete(Pid) ->
	log4erl:info("delete()~p)",[Pid]),
	case config:get(table_type) of 
		{ok,ets} ->
			RetVal = ets_store:delete(Pid);
		{ok,mnesia} ->
			RetVal = mnesia_store:delete(Pid);
		{error, Reason} ->
			RetVal = {error, Reason}
	end,
	RetVal.

stop() ->
	log4erl:info("Stop the store"),
	case config:get(table_type) of 
		{ok,ets} ->
			RetVal = ets_store:stop();
		{ok,mnesia} ->
			RetVal = mnesia_store:stop();
		{error, Reason} ->
			RetVal = {error, Reason}
	end,
	RetVal.

%% ====================================================================
%% Internal functions
%% ====================================================================


	

