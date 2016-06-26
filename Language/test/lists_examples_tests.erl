%%%-------------------------------------------------------------------
%%% @author Roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc This is the first tests module I've written in ages, to test the Lists - example.
%%%
%%% @end
%%% Created : 26. Jun 2016 21:26
%%%-------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

-module(lists_examples_tests).
-author("Roger").

%% API
-export([]).

head_add_tail_test() ->
  ?assertEqual(6,lists_examples:head_add_tail()).

