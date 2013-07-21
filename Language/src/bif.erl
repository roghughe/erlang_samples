%% @author Roger
%% @doc Some example functions that use BIFs


-module(bif).

%% ====================================================================
%% API functions
%% ====================================================================
-export([my_hd/1,my_tl/1,my_tuple_size/1,my_element/2,
		 my_length/1,my_append_element/2,my_atom_to_list/1,
		 my_list_to_atom/1,
		 my_date/0,my_time/0,my_now/0]).

%% Head BIF example
my_hd(List) ->
	hd(List).

%% Tail BIF example
my_tl(List) ->
	tl(List).

%% Length of a list example
my_length(List) ->
	length(List).

%% Tuple size example
my_tuple_size(Tuple) ->
		tuple_size(Tuple).

%% Element example
my_element(Tuple,Pos) ->
	element(Tuple,Pos).

%% append_element example
my_append_element(Tuple, Term) ->
	erlang:append_element(Tuple, Term).

%% ====================================================================
%% Conversion Examples
%%
%% See also: list_to_tuple/1, tuple_to_list/1, float/1, list_to_float/1
%% integer_to_list/1 etc.
%% ====================================================================

%% atom_to_list example
my_atom_to_list(Atom) ->
	atom_to_list(Atom).

%% list to atom example
my_list_to_atom(List) ->
	list_to_atom(List).


%% ====================================================================
%% System Information Examples
%% ====================================================================

%% Returns Tuple {Year,Month,Date}
my_date()->
	date().

%% Returns Tuple {Hour,Minutes,Seconds}
my_time() ->
	time().

%% MegaSeconds, Seconds, MicoSeconds since 1st Jan 1970 
my_now() ->
	now().


%% ====================================================================
%% Internal functions
%% ====================================================================


