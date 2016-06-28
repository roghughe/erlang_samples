%%%-------------------------------------------------------------------
%%% @author Roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc These are a few basic list examples
%%%
%%% @end
%%% Created : 26. Jun 2016 20:39
%%%-------------------------------------------------------------------
-module(lists_examples).
-author("Roger").

%% API
-export([head_add_tail/0,find_head_example/0]).

%% @doc This is a simple list head tail example
head_add_tail() ->

  List = [2,3,6,7,8],

  Head = [0,1],

  %% This combines the two lists
  NewList = [Head|List],

  %% Write the output to the io (screen)
  io:fwrite("The new list is length: ~p ", [length(NewList)]),

  %% This is what gets returned
  length(NewList).


%% @doc Entry point for this example - it's nothing special.
%% Note that this code is more verbose than it needs to be
find_head_example() ->

  List = [1,2,3,4],
  Head = head(List),
  Head2 = head2(List),

  % Remember - this is the equivalent to ==
  Head =:= Head2.

%% Note that the 'doing' part of the method is in the argument here
head([H|_]) -> H.

%% This should be the same as the above
head2(List) ->
  [H|_] = List,
  H.

