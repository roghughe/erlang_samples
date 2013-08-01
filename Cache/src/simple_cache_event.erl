%% @author Roger
%% @doc Wrapper for gen_event calls. Called when something happens in our internal cache


-module(simple_cache_event).

-export([lookup/1, delete_handler/2, add_handler/2, start_link/0,create/2,replace/2,delete/1]).

-define(SERVER,?MODULE).

%% ====================================================================
%% Lifecycle functions
%% ====================================================================

%% @doc Create an new event stream for this module
start_link() ->
	gen_event:start_link({local,?SERVER}).

%% @doc Add an handler for this event stream
add_handler(Handler,Args) ->
	gen_event:add_handler(?SERVER, Handler, Args).

%% @doc Delete a handler from this stream
delete_handler(Handler,Args) ->
	gen_event:delete_handler(?SERVER, Handler, Args).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Call this when a lookup event takes place
lookup(Key) ->
	gen_event:notify(?SERVER, {lookup,Key}).

%% @doc Call this when a create event takes place
create(Key,Value) ->
	gen_event:notify(?SERVER, {create, {Key,Value}}).

%% @doc Call this when a replace event takes place
replace(Key,Value) ->
	gen_event:notify(?SERVER, {replace, {Key,Value}}).
	
%% @doc Call this when a delete event takes place
delete(Key) ->
	gen_event:notify(?SERVER, {delete,Key}).

%% ====================================================================
%% Internal functions
%% ====================================================================


