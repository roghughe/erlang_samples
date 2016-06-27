%% @author Roger
%% @doc @todo Add description to bitstring.


-module(bitstring).

%% ====================================================================
%% API functions
%% ====================================================================
-export([ex1/0,ex2/0,ex3/0,ex4/0,ex5/0,ex6/0,ex7/0,ex8/0]).

%% @doc Each integer has a range of 0 - 255 Larger values are truncated
%% &lt;&lt;0,254,255,256,257&gt;&gt;. becomes  &lt;&lt;0,254,255,0,1&gt;&gt;
ex1() ->
	 <<0,254,255,256,257>>.

ex2() ->
	 <<"Roger">>.

%% @doc Don't forget  <<$R,$o,$g,$e,$r>> is really  <<"Roger">>
ex3() ->
	 <<$R,$o,$g,$e,$r>>.

%% @doc Concatenate 2 bit strings using the bits TypeDef
ex4() ->
	A = <<1,2>>,
	B = <<3,4>>,

	<<A/bits, B/bits>>.
	% <<A,B>>. - won;t work.
	% See: http://www.erlang.org/doc/reference_manual/expressions.html#id78513

%% @doc How to filter bits out of bitstring
%% Think about this. The test expects Version = 2 and Size = 0. We're using 4 bits for Version and 4 for Size, therefore
%% for the value of 32 we have, in terms of 8 bits 0010 0000 - hence Version = 0010 = 4 and Size = 0000 = 0
ex5() ->
	<<Version:4, Size:4>> = <<32>>,
	{Version,Size}.
	

%% @doc Bitstring comprehension
%% Packs the numbers in a list at 3 bits per integer (X:3) in to a bitstring 
%% returns <<41,203,23:5>>, where the default size is 8 and the last size is 5 - 23:5
%% i.e. the total number of bits is 21 (3 bits per number * 7 numbers in the list)
ex6() ->
	
	MyList = [1,2,3,4,5,6,7],

	%% 001 010 011 100 101 110 111
	%% This has a length of 21 bits
	%% Which is 8 bits + 8 bits + 5 bits (8 is the default size)
	%% Hence the first number is
	%%    00101001 = 41
	%% and
	%%    11001011 = 201
	%% and
	%%    10111 = 23
	%% expected output is <<41,203,23:5>>
	Out = << <<X:3>> || X <- MyList >>,
	io:fwrite("Look:  ~p~n",[Out]),
	Out.


%% @doc Convert the bitstring, into a 1 - 7 bitstring
%% Output: &lt;&lt;1,2,3,4,5,6,7&gt;&gt;
ex7() -> 
	MyBitStream = <<41,203,23:5>>,
	<< <<X:8>> || <<X:3>> <= MyBitStream >>.

%% @doc Convert the bitstring back into a list using a List comprehension
%% Output: [1,2,3,4,5,6,7]
ex8() ->
	MyBitStream = <<41,203,23:5>>,
	[ X || <<X:3>> <= MyBitStream ].
	

