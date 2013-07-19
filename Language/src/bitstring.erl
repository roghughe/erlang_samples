%% @author Roger
%% @doc @todo Add description to bitstring.


-module(bitstring).

%% ====================================================================
%% API functions
%% ====================================================================
-export([ex1/0,ex2/0,ex3/0,ex4/0,ex5/0,ex6/0,ex7/0,ex8/0]).

%% Each integer has a range of 0 - 255 Larger values are truncated
%%  <<0,254,255,256,257>>. becomes  <<0,254,255,0,1>>
ex1() ->
	 <<0,254,255,256,257>>.

ex2() ->
	 <<"Roger">>.

%% Don't forget  <<$R,$o,$g,$e,$r>> is really  <<"Roger">>.
ex3() ->
	 <<$R,$o,$g,$e,$r>>.

%% Concatenate 2 bit strings using the bits TypeDef
ex4() ->
	A = <<1,2>>,
	B = <<3,4>>,

	<<A/bits, B/bits>>.
	%% <<A,B>>. - won;t work.
	%% See: http://www.erlang.org/doc/reference_manual/expressions.html#id78513

%% How to filtre bits out of bitstring
ex5() ->
	<<Version:4, Size:4>> = <<32>>,
	io:fwrite("Version: ~w  Size: ~w~n",[Version,Size]).

%% Bitstring comprehension
%% Packs the numbers in a list at 3 bits per integer (X:3) in to a bitstring 
%% returns <<41,203,23:5>>, where the default size is 8 and the last size is 5 - 23:5
%% i.e. the total number of bits is 21 (3 bits per number * 7 numbers in the list)
ex6() ->
	
	MyList = [1,2,3,4,5,6,7],
	<< <<X:3>> || X <- MyList >>.

%% Convert the bitstring, into a 1 - 7 bitstring
%% Output: <<1,2,3,4,5,6,7>>
ex7() -> 
	MyBitStream = <<41,203,23:5>>,
	<< <<X:8>> || <<X:3>> <= MyBitStream >>.

%% Convert the bitstring back into a list using a List comprehension
%% Output: [1,2,3,4,5,6,7]
ex8() ->
	MyBitStream = <<41,203,23:5>>,
	[ X || <<X:3>> <= MyBitStream ].
	

%% ====================================================================
%% Internal functions
%% ====================================================================


