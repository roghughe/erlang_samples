%%%-------------------------------------------------------------------
%%% @author Roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc This just demonstrates the use of type conversion functions and data type guards
%%%
%%% @end
%%% Created : 30. Jun 2016 13:15
%%%-------------------------------------------------------------------
-module(types).
-author("Roger").

%% API
-export([conversion_examples/0,guard_examples/0]).

%%
%%  @doc There are lots of conversion functions. These are not all of them
%%
conversion_examples() ->

  L1 = integer_to_list(54),
  F1 = list_to_float("54.5"),
  B1 = list_to_binary(L1),
  I1 = list_to_integer(L1),
  T1 = list_to_tuple(L1),

  {L1,F1,B1,I1,T1}.


guard_examples() ->

  boolean_guard(true),
  boolean_guard(false),

  % This is not a good thing to do, but demonstrates the fact that the guarded method will fail for the wrong type argument
  try
    boolean_guard(wibble)
  catch
      _:_ -> io:format("Catch All~n")
  end,

  atom_guard(true),
  atom_guard(wibble),
  try
    atom_guard("true")
  catch
    _:_ -> io:format("Catch All~n")
  end,

  try

    atom_guard(54)
  catch
  _:_ -> io:format("Catch All~n")
  end,

  list_guard("124"),
  list_guard([1,2,3,4]),

  integer_guard(45),

  ok.

boolean_guard(A) when is_boolean(A) ->
  do_something_here.

atom_guard(A) when is_atom(A) ->
  do_something_here.

list_guard(A) when is_list(A) ->
  do_something_here.

integer_guard(A) when is_integer(A) ->
  do_something_here.
