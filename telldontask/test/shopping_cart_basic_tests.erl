%% @author Roger
%% @doc Example eunit test that corresponds to its Java equivalent

-include_lib("eunit/include/eunit.hrl").

-module(shopping_cart_basic_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% @doc Calculate total code - written to match the Java style
calculate_total_cost_test() ->
	
	EmptyList = [],
	OrderList1 = shopping_cart_basic:add_item(EmptyList,{gloves,23.43}),
	OrderList2 = shopping_cart_basic:add_item(OrderList1,{hat,10.99}),
	OrderList3 = shopping_cart_basic:add_item(OrderList2,{scarf,5.99}),

	?_assertEqual(40.42,shopping_cart_basic:calc_total_cost(OrderList3)).

%% @doc Calculate total cost example - written in a better erlang style
calculate_total_cost_2_test() ->

	OrderList = [{gloves,23.43},{hat,10.99},{scarf,5.99}],
	?assertEqual(40.41,shopping_cart_basic:calc_total_cost(OrderList)).


%% ====================================================================
%% Internal functions
%% ====================================================================


