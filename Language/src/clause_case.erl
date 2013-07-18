%% @author Roger
%% @doc Demonstrates function clauses and equivalent(?) implementation using a case expression.
 


-module(clause_case).

%% ====================================================================
%% API functions
%% ====================================================================
-export([either_or_both1/2, either_or_both2/2, area1/1,area2/1]).

%% Function Clause with a guard
either_or_both1(true,A) when is_boolean(A) ->
	true;
either_or_both1(A,true) when is_boolean(A) ->
	true;
either_or_both1(false,false)  ->
	false.

%% Same as either_or_both, but implemented using a Case expression
either_or_both2(A,B) ->
	
	% Make a tuple from the args so you can match...
	case{A,B} of 
		{true,B} when is_boolean(B) ->
			true;
		{A,true} when is_boolean(A) ->
			true;
		{false,false} ->
			false
	end.


%% Implement the area of a shape using clauses
area1({circle,Radius}) ->
	Radius * Radius * math:pi();
area1({square,Side}) ->
	Side * Side;
area1({rectangle, Height,Width}) ->
	Height * Width.


%% Implement the area of a shape using a case
area2(Shape) when is_tuple(Shape) ->
	case Shape of
	{circle, Radius} ->
		Radius * Radius * math:pi();
 	{square,Side} ->
		Side * Side;
	{rectangle,Height,Width} ->
		Height * Width
	end.




%% ====================================================================
%% Internal functions
%% ====================================================================


