%% @author Roger
%% @doc These are a few list comprehension examples


-module(list_comp).

%% ====================================================================
%% API functions
%% ====================================================================
-export([basic_list_comprehension/0,ex2/0,ex3/0]).

%% @doc Basic list comprehension. Return a sub-list of all numbers less then 6
basic_list_comprehension() ->
	%% Define a list of integers
	MyList = [0,1,2,3,4,5,6,7,8,9,10],

	%% Define the list comprehension
	[X || X <- MyList, X < 6].

%% @doc Example list comprehension that squares all even values less than 6
ex2() ->
	MyList = [0,1,2,3,4,5,6,7,8,9,10],

	%% math:pow(X,2) - template or what to do with the filtred X value
	%% X <- MyList - push each value in the list into X
	%% X < 6, X rem 2 == 0 - filtre conditions 
	[math:pow(X,2) || X <- MyList, X < 6, X rem 2 == 0].

%% @doc Applying List Comprehension to tuples - with filtering
%% Calculate the area of all tuples that are tagged with a 'rectangle' and have an area greater than 50
ex3() ->
	Shapes = [{rectangle,100,200},{rectangle,10,2},{square,25},{rectangle,30,100}],
	
	[{area, H*W} || {rectangle, H,W} <- Shapes, H*W >=50].


%% ====================================================================
%% Internal functions
%% ====================================================================


