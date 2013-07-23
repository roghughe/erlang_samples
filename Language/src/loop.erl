%% @author Roger
%% @doc Example that demonstrates how to iterate over a list using recursion.
%% The key is to split the list using [Value | TheRest] where 'TheValue' will be the 
%% first value in the list and 'TheRest' are the other values.


-module(loop).

%% ====================================================================
%% API functions
%% ====================================================================
-export([render/1,to_html/1,sum/1,sum2/1,sum3/1,reverse/1,tail_reverse/1,contains/2,sum4/1]).

	
%% @doc Render the HTML to display an unordered list
%% Guard the input arg at the highest level
render(Items) when is_list(Items) ->
	Out = to_html(Items),
	io:fwrite(Out).


%% @doc create an HTML unordered list from the list arg
to_html(Items) ->
	"<ul>~n" ++ item(Items) ++ "</ul>~n".

%% @doc Format the fist item in the list into a list item
%% Recursively call item(...) for each successive list value
%% Function clause is chosen by pattern matching
item([Item | TheRest]) ->
	"<li>" ++ Item ++ "</li>~n" ++ item(TheRest);
item([]) -> "".

%% @doc Wrapper function for summing
sum(Number) ->
	sum(Number,0).

%% @doc recursive function for summing...
%% Function clause is chosen using the guard
%% This is tail recursive
sum(N,Total) when N =/= 0 ->
	sum(N-1, Total + N);
sum(0,Total) ->
	Total.


%% @doc Wrapper function for summing
sum2(Number) ->
	sum2(Number,0).

%% @doc recursive function for summing...
%% Function clause is chosen using pattern matching.
%% Note the ordering of the cluases is important when not using guards
sum2(0,Total) ->
	Total;
sum2(N,Total)  ->
	sum(N-1, Total + N).

%% @doc recursive function for summing...
%% Non tail recursive
sum3(0) ->	0;
sum3(N) ->
	sum(N-1) + N.

%% @doc Reversing a list using recursion
%% This is not so hot because of its quadratic execution time
reverse([]) ->
	[];
reverse([X]) ->
	[X];
reverse([Item | TheRest]) -> 
	reverse(TheRest) ++ [Item].


%% @doc Reversing a list using recursion
%% This is better because of its linear execution time
tail_reverse(List) ->
	tail_reverse(List,[]).

tail_reverse([Item | TheRest], Result) ->
	tail_reverse(TheRest,[Item | Result]);
tail_reverse([],Result) ->
	Result.

%% @doc function to determine if a list contains a value
%% non tail recursive in quadratic time - but does it matter it's not returning the 
%% list so doesn't the compiler optimise stuff? @todo check this out
contains([Item | _],Value) when Item =:= Value ->
	true;
contains([Item | TheRest], Value) when Item =/= Value ->
	contains(TheRest,Value);
contains([],_) ->
	false.

%% @doc Beware of end points, you may need to consider the data in your list/tuple etc when operating on it. 
%% This example sum() method uses the length of the list rather than ZERO
sum4(List) ->
	sum4(List, length(List), 0).

%% @docTail recursive sum of an array
sum4([Item | TheRest],Length,Total) when Length > 0 ->
	sum4(TheRest, Length -1,Total + Item);
sum4(_,0,Total) ->
	Total.



