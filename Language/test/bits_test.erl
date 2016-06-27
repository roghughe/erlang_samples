%%%-------------------------------------------------------------------
%%% @author Roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc This is a sample file that works with binary data
%%%
%%% @end
%%% Created : 27. Jun 2016 17:05
%%%-------------------------------------------------------------------
-module(bits_test).
-author("Roger").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).

%% Check the output as a 24 bit field, for our colour orange...
orange_test() ->
  ?assertEqual(<<240,154,41>>,bits:orange()).