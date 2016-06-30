%%%-------------------------------------------------------------------
%%% @author Roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc This file demonstrates how to do an 'if' statement, how to use compiler options, the use of 'guards'
%%%
%%% @end
%%% Created : 28. Jun 2016 22:08
%%%-------------------------------------------------------------------
-module(if_example).
-author("Roger").

%% API
-export([compare_numbers/0,compare_numbers/2,compare_once_more/2,compare_chars/1,
  valid_time/1,valid_time_guard/1,compare_numbers_alt_but_better/2]).

-ifdef(TEST).
-export([valid_time_test/0,valid_time_test_fail/0]).
-endif.

compare_numbers() ->

  compare_numbers(1,2),
  compare_numbers_alt_but_better(3,4),
  compare_once_more(3,4),
  compare_chars('G'),
  A = 'A',
  compare_chars(A),
  ok.


%% @doc This compares two numbers
%% Remember for Boolean operators we have...  =:= and == where == ignores type
%% =:= =|=  /=
compare_numbers(A,B) ->

  if
    A =:= B ->
      match;
    true ->
      nomatch
  end.

compare_numbers_alt_but_better(A,B) ->

  if
    A =:= B ->
      match;
    A =/= B ->
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

%% This is how you use conditional compilation to add in test functions
-ifdef(TEST).
valid_time_test() ->
  valid_time_guard({20,30,10}).

valid_time_test_fail() ->
  valid_time_guard({28,34,44}).
-endif.


%% formats some time from a tuple of date/time values
valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
  io:format("The Date Tuple (~p) says today is: ~p/~p/~p~n",[Date,Y,M,D]),
  io:format("The Time Tuple (~p) says today is: ~p:~p:~p~n",[Time,H,Min,S]);
valid_time(_) ->
  io:format("Invalid data").

%% This function applies a guard to the function, validating the Time tuple before its passed to the io:format()
valid_time_guard(Time = {H,Min,S}) when H >= 0, H < 24, Min >= 0, Min < 60,  S >= 0, S < 60 ->
  io:format("The Time Tuple (~p) says today is: ~p:~p:~p~n",[Time,H,Min,S]);
valid_time_guard(_) ->
  io:format("Invalid time").
