%% @author Roger
%% @doc Covers the very basics - "hello world" - plus:
%% pattern matching
%% string concatention
%% _ as a wildcard character
%% _ as the first char of a variable name
%% how to return a value from a function
%% function 'Guards'

-module(hello).

%% ====================================================================
%% API functions
%% ====================================================================
-export([hello_world/0,
		 greet/1,
		 height_from_point/1,
		 callsFunction/1,
		 reverse_list/1,
		 all/3,
		 eitherOrBoth/2,
	   all/0]).

all() ->
	all("Roger",{rectangle,23,4},[1,2,3,4,5,6]).


all(Name,Point,List) ->
	hello_world(),
	greet(Name),
	callsFunction(Name),
	Height = height_from_point(Point),
	reverse_list(List),
	Height.


%% @doc Hello World
hello_world() ->
	io:fwrite("This is going to print \"Hello World\"~n"),
	io:fwrite("Hello World!~n").

%% @doc Simple hello_world + 1 application
greet(Name) ->
	Out = "Hello " ++ Name,  		% Concatenate Strings
	io:fwrite("~p~n",[Out]).


%% @doc Returns the Height value of the point Tuple
%% Example of Pattern matching
height_from_point(Point) ->
	{rectangle,Height,_} = Point,	% _ means wildcard in this case
	% Return the height part of the tuple
	Height.	                        

%% @doc Example of calling another function
callsFunction(Name) ->
	_Unused = 10,            % Unused, but the _ means no warning
	Out = concat_string("Hello ", Name),  % Concatenate Strings
	write(Out).

%% @doc Calls the standard reverse function in the lists module       
reverse_list(List) ->
	lists:reverse(List).

%% @doc Function Clause with a guard
eitherOrBoth(true,A) when is_boolean(A) ->
	true;
eitherOrBoth(A,true) when is_boolean(A) ->
	true;
eitherOrBoth(false,false)  ->
	false.




%% ====================================================================
%% Internal functions
%% ====================================================================

write(Out) ->
	io:fwrite("~p~n",[Out]).

%% @doc Concat two String, with a not very good way of guarding in that Strings are a specific type of list rather than a general data type
%% Also see: is_boolean(), is_integer(), is_atom() - 
%% See http://en.wikibooks.org/wiki/Erlang_Programming/guards
concat_string(A,B) when is_list(A), is_list(B) ->
	A ++ B.

