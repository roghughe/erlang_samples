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

to_html_test() ->
	Expected = "<ul>~n<li>Fred</li>~n<li>Bill</li>~n<li>Jim</li>~n<li>Tim</li>~n</ul>~n",
	?assertEqual(Expected,loop:to_html(["Fred","Bill","Jim","Tim"])).

sum_test() ->
	?assertEqual(55,loop:sum(10)).

sum2_test() ->
	?assertEqual(55,loop:sum2(10)).

sum3_test() ->
	?assertEqual(55,loop:sum3(10)).

reverse_test_() ->
	[?_assertEqual([q,w,e,r,t,y],loop:reverse([y,t,r,e,w,q])),
	?_assertEqual([q],loop:reverse([q])),
	?_assertEqual([],loop:reverse([])),
	?_assertEqual([q,w],loop:reverse([w,q]))].

tail_reverse_test_() ->
	[?_assertEqual([q,w,e,r,t,y],loop:tail_reverse([y,t,r,e,w,q])),
	?_assertEqual([q,w],loop:tail_reverse([w,q])),
	?_assertEqual([q],loop:tail_reverse([q])),
	?_assertEqual([],loop:tail_reverse([]))].


sum4_test_() ->
	[?_assertEqual(10,loop:sum4([1,2,3,4])),
	 ?_assertEqual(8,loop:sum4([-1,2,3,4])),
	 ?_assertEqual(0,loop:sum4([])),
	 ?_assertEqual(0,loop:sum4([0])),
	 ?_assertEqual(10,loop:sum4([10]))].


