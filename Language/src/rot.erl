%%%-------------------------------------------------------------------
%%% @author Roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc Simple demonstration of ROT encoding
%%%
%%% @end
%%% Created : 30. Jun 2016 17:46
%%%-------------------------------------------------------------------
-module(rot).
-author("Roger").

%% API
-export([rot/2, rot_start/0]).

%% This is how to do Macros - use them by adding a ? eg: ?A
-define(A,65).  % Capital A
-define(Z,65+26).  % Capital Z
-define(a,97).  % Lowercase a
-define(z,97+26).  % Lowercase z
-define(ALPHABET_LENGTH,26).

%% Dummy entry point for testing
rot_start() ->
  io:fwrite("Result ~p~n",[rot("Roger",13)]).

%% @doc This is the main function, taking  string and encoding it.
rot(String, Rot) when is_list(String), is_integer(Rot) ->
  rot(String,Rot,[]).

%% @doc This is where you use recursion to loop through the values in your string.
rot([Item|Rest],Rot,Result) ->
  rot(Rest,Rot,[encode(Item,Rot)|Result]);
rot([],_,Result) ->
  lists:reverse(Result).

%% @doc Using a guard, choose the right encode function.
encode(Abyte,Rot) when Abyte >= ?A, Abyte =< ?Z ->
  encode(Abyte,?A, Rot);
encode(Abyte,Rot) when Abyte >= ?a, Abyte =< ?z ->
  encode(Abyte,?a, Rot);
encode(Abyte,_)  ->
  Abyte.

%% @doc This is the bit that does the actual encoding. Given abyte, the start character of the alphabet  (either 'a' or 'A')
%% and the rotation value, return an encoded value
encode(Abyte,AlphaStart,Rot) ->
  Tmp = (Abyte - AlphaStart + Rot),
  Tmp2 = mod(Tmp,?ALPHABET_LENGTH) + AlphaStart,
  Tmp2.

%% Taken from Stack overflow: the bet way to do modulo.
mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.
