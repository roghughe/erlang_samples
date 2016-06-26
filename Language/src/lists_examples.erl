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
-export([head_add_tail/0]).

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


