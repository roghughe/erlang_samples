%% @author Roger
%% @doc Test looping using recursion

-include_lib("eunit/include/eunit.hrl").

-module(loop_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% Test method for the rendering of HTML
render_test() ->
	?assertEqual(ok,loop:render(["Fred","Bill","Jim","Tim"])).


%% ====================================================================
%% Internal functions
%% ====================================================================

to_html_test() ->
	Expected = "<ul>~n<li>Fred</li>~n<li>Bill</li>~n<li>Jim</li>~n<li>Tim</li>~n</ul>~n",
	?assertEqual(Expected,loop:to_html(["Fred","Bill","Jim","Tim"])).

