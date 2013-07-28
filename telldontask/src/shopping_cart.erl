%% @author Roger
%% @doc @todo Add description to shopping_cart_basic.


-module(shopping_cart).

%% ====================================================================
%% API functions
%% ====================================================================
-export([add_item/2,calc_total_cost/1,calc_total_cost/3,pay/3]).

%% @doc Add an item to the order list
add_item(OrderList,Item) ->
	[Item | OrderList].

%% @doc Calculate the total cost of all the items in a list. The List must have the following format:
%%  [{itemName, Price}]
%%  where 
%%  itemName -> atom
%%  Price -> float()
calc_total_cost(OrderList) ->
	round_dp(calc_total_cost(0,OrderList)).

%% @doc Calculate the total cost of all the items in a list adding a shipping cost if the value is below a certain limit.
%% The Order List must have the following format:
%%  [{itemName, Price}]
%%  where 
%%  itemName -> atom
%%  Price -> float()
calc_total_cost(OrderList,Shipping, MinShippingAmount) ->
	Cost = calc_total_cost(OrderList),
	TotalCost = Cost + shipping(Cost,Shipping,MinShippingAmount),
	round_dp(TotalCost).

%% @doc @todo Method not implemented
pay(_Order,_Shipping, _MinShippingAmount) ->
	unimplemented.

%% ====================================================================
%% Internal functions
%% ====================================================================

calc_total_cost(Result,[{_,Price} | TheRest]) ->
	calc_total_cost(Result + Price,TheRest);
calc_total_cost(Result,[]) ->
	Result.

shipping(Cost,Shipping,MinShippingAmount) when Cost < MinShippingAmount ->
	Shipping;
shipping(_,_,_) ->
	0.

round_dp(Number) ->
	List = float_to_list(Number,[{decimals,2}]),
	list_to_float(List).
