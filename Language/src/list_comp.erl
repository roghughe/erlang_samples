%% @author Roger
%% @doc @todo Add description to list_comp.


-module(list_comp).

%% ====================================================================
%% API functions
%% ====================================================================
-export([ex1/0,ex2/0,ex3/0]).

%% Basic list comprehension. Return a sub-list of all numbers less then 6
ex1() ->
	%% Define a list of integers
	MyList = [0,1,2,3,4,5,6,7,8,9,10],

	%% Define the list comprehension
	[X || X <- MyList, X < 6].

%% Example list comprehension that squares all even values less than 6
ex2() ->
	MyList = [0,1,2,3,4,5,6,7,8,9,10],

	%% math:pow(X,2) - template or what to do with the filtred X value
	%% X <- MyList - push each value in the list into X
	%% X < 6, X rem 2 == 0 - filtre conditions 
	[math:pow(X,2) || X <- MyList, X < 6, X rem 2 == 0].

%% Applying List Comprehension to tuples - with filtering
%% All tuples that are tagged with a 'rectangle' and have an area greater than 50
ex3() ->
	Shapes = [{rectangle,100,200},{rectangle,10,2},{square,25},{rectangle,30,100}],
	
	[{area, H*W} || {rectangle, H,W} <- Shapes, H*W >=50].


%% ====================================================================
%% Internal functions
%% ====================================================================

