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
-export([compare_numbers/0,compare_numbers/2,compare_once_more/2,compare_chars/1]).

compare_numbers() ->

  compare_numbers(1,2),
  compare_once_more(3,4),
  compare_chars('G'),
  A = 'A',
  compare_chars(A),
  same(2,2),
  same(1,2),
  ok.


%% @doc This compares two numbers
%% Remember =:= and ==
compare_numbers(A,B) ->

  if
    A =:= B ->
      match;
    true ->
      nomatch
  end.

compare_once_more(A,B) ->

  if
    A =:= B ->
      equal;
    A >= B ->
      greaterThan;
    A =< B ->
      lessThan
  end.

compare_chars(A) ->

  if
    A =:= 'A' ->
      a;
    A =:= 'B' ->
      b;
    A =:= 'C'->
      c;
    true ->
      nomatch
  end.

%% This looks like the more erlang way - pattern matching
same(X,Y) ->
  true;
same(_,_) ->
  false.
