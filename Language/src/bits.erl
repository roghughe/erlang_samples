%%%-------------------------------------------------------------------
%%% @author Roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jun 2016 16:58
%%%-------------------------------------------------------------------
-module(bits).
-author("Roger").

%% API
-export([orange/0]).

%% @doc Put the binary value of F09A29 on 24 bits of space (RGB are each 8 bits). This can then be put
%% into a file or socket later. Apparently, it's important.
orange() ->

  Colour = 16#F09A29,

  Pixel = <<Colour : 24>>,
  Pixel.
