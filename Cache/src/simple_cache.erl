%% @author Roger
%% @doc This is the external API for the application containing the usual suspects: insert, lookup and delete.


-module(simple_cache).

%% ====================================================================
%% API functions
%% ====================================================================
-export([delete/1, lookup/1, insert/2]).


%% @doc Insert function. combines both insert and update (i.e. if the item already exists then do an update...)
insert(Key,Value) ->
	log4erl:info("Simple Cache API - insert called"),
	case simple_cache_store:lookup(Key) of 
		{ok,Pid} ->
			simple_cache_element:replace(Pid,Value),
			simple_cache_event:replace(Key,Value);
		{error,_} ->
			{ok, Pid} = simple_cache_element:create(Value),
			simple_cache_store:insert(Key, Pid),
			simple_cache_event:create(Key,Value)
	end.


lookup(Key) ->
	log4erl:info("Simple Cache API - lookup called"),
	try 
		{ok, Pid} = simple_cache_store:lookup(Key),
		{ok, Value} = simple_cache_element:fetch(Pid),
		simple_cache_event:lookup(Key),
		{ok,Value}
	catch
		_Class:_Exception ->
			{error, not_found}
	end.

delete(Key) ->
	log4erl:info("Simple Cache API - delete called"),
	case simple_cache_store:lookup(Key) of 
		{ok,Pid} -> 
			simple_cache_element:delete(Pid),
			simple_cache_event:delete(Key);
		{error,_Reason} ->
			ok
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================


