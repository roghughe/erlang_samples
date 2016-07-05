%% @author Roger
%% @doc Demostrate the fun sample code


-module(funs_tests).

-include_lib("eunit/include/eunit.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

anon_fun_test() ->
	?assertEqual(ok,funs:anon_fun()).

anon_fun2_test() ->
	?assertEqual(ok,funs:anon_fun2()).

anon_fun3_test() ->
	?assertEqual(ok,funs:anon_fun3()).

local_fun_test() ->
	?assertEqual(ok,funs:local_fun()).

remote_module_fun_test() ->
	?assertEqual(ok,funs:remote_module_fun()).

to_html_test() ->
	Text = "Hello",
	Fun = fun() ->
			"<b>" ++ Text ++ "</b>"
		  end,

	?assertEqual("<p><b>Hello</b></p>~n",funs:to_html(Fun)).

render_test() ->
	?assertEqual(ok,funs:render("Hello", "b")).


demo_alarm_test() ->
	?assertEqual(ok,funs:demo_alarm()).

%% This is an example of a 'map' function (something that acts on elements of a collection to change them i.e. map them
%% to something else..
%% The arguments are a function declaration 'fun funs:incr/1' and a list of numbers to increment.
map_test() ->
	List = [1,2,3],
	?assertEqual([2,3,4],funs:map(fun funs:incr/1, List)).

%% This is the same as the above - except that it uses an anonymous function
map_for_anonymous_func_test() ->
	List = [1,2,3],
	?assertEqual([2,3,4],funs:map(fun(X) -> X+1 end, List)).

%% Erlang contains an built in map function.
map_for_anonymous_func_api_test() ->
	List = [1,2,3],
	?assertEqual([2,3,4],lists:map(fun(X) -> X+1 end, List)).

% Filter function test
filter_test() ->
	List = [2,3,4,5,6,7,8],
	?assertEqual([2,4,6,8],funs:filter(fun(X) -> X rem 2 == 0 end, List)).

% Filter function test - showing that erlang already has such a function
filter_using_api_test() ->
	List = [2,3,4,5,6,7,8],
	?assertEqual([2,4,6,8],lists:filter(fun(X) -> X rem 2 == 0 end, List)).

fold_max_test() ->
	List = [1,5,3,6,4,2],
	?assertEqual(6,funs:fold(fun(A,B) when A > B -> A; (_,B) -> B end,0, List)).

fold_sum_test() ->
	List = [1,2,3,4],
	?assertEqual(10,funs:fold(fun(A,B) -> A + B end,0, List)).

fold_sum_api_test() ->
	List = [1,2,3,4],
	?assertEqual(10,lists:foldl(fun(A,B) -> A + B end,0, List)).



%% ====================================================================
%% Internal functions
%% ====================================================================


