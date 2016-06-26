%% @author Roger
%% @doc Samples demonstrating the use of Built in Functions (Bifs).
%% For more info see http://www.erlang.org/doc/man/erlang.html

-include_lib("eunit/include/eunit.hrl").

-module(bif_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% Test the hd (head) function - returns the first element of a list
hd_test() ->
	
	Result = bif:my_hd([9,8,7,6,5,4]),
	?assertEqual(9,Result).

%% Test the tl function - tl returns the rest of a list (tail) after removing the first element
tl_test() ->
	Result = bif:my_tl([1,2,3,4,5,6]),
	?assertEqual([2,3,4,5,6],Result).

%% Test the length function
length_test() ->
	?assertEqual(4,bif:my_length([1,2,3,4])).

%% tuple_size test
tuple_size_test() ->
	?assertEqual(3,bif:my_tuple_size({roger,wilco,overAndOut})).

%% element Test - Remember that it starts at 1 - not 0
element_test() ->
	?assertEqual(wilco,bif:my_element(2,{roger,wilco,overAndOut})).

%% append_element test
append_element_test() ->
	?assertEqual({alpha,beta,gamma},bif:my_append_element({alpha,beta},gamma)).

%% atom to list test (Note the atom Roger is surrounded by '' because of the first capital letter)
atom_to_list_test() ->
	?assertEqual("Roger",bif:my_atom_to_list('Roger')).

%% list to atom test
list_to_atom_test() ->
	?assertEqual('Roger',bif:my_list_to_atom("Roger")).


%% test getting the date 
date_test() ->
	{Year,Month,Day} = bif:my_date(),
	io:format("The date is: ~p/~p~p~n",[Day,Month,Year]),
	?assertEqual(2016,Year).

%% ====================================================================
%% Internal functions
%% ====================================================================


