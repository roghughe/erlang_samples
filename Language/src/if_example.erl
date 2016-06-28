%%%-------------------------------------------------------------------
%%% @author Roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc This file demonstrates how to do an 'if' statement
%%%
%%% @end
%%% Created : 28. Jun 2016 22:08
%%%-------------------------------------------------------------------
-module(if_example).
-author("Roger").

%% API
-export([compare_numbers/0,compare_numbers/2]).

compare_numbers() ->

  compare_numbers(1,2).


%% @doc This compares two number
%% Remember =:= and ==
compare_numbers(A,B) ->

  if
    A =:= B ->
      match;
    true ->
      nomatch
  end.

