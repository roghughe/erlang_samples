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

	
	


%% ====================================================================
%% Internal functions
%% ====================================================================


