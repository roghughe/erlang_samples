%% @author Roger
%% @doc Example that demonstrates how to iterate over a list using recursion.
%% The key is to split the list using [Value | TheRest] where 'TheValue' will be the 
%% first value in the list and 'TheRest' are the other values.


-module(loop).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test_render/0,render/1]).

%% Test method for the rendering of HTML
test_render() ->
	render(["Fred","Bill","Jim","Tim"]).
	
%% Render the HTML to display an unordered list
%% Guard the input arg at the highest level
render(Items) when is_list(Items) ->
	Out = to_html(Items),
	io:fwrite(Out).


%% create an HTML unordered list from the list arg
to_html(Items) ->
	"<ul>~n" ++ item(Items) ++ "</ul>~n".

%% Format the fist item in the list into a list item
%% Recursively call item(...) for each successive list value
item([Item | TheRest]) ->
	"<li>" ++ Item ++ "</li>~n" ++ item(TheRest);
item([]) -> "".


%% ====================================================================
%% Internal functions
%% ====================================================================


