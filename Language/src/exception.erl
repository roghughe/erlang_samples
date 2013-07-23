%% @author Roger
%% @doc @todo Add description to exception.


-module(exception).

%% ====================================================================
%% API functions
%% ====================================================================
-export([catch_all/0,catch_me/1,tryof/1,after_clause/0,stack/1]).

%% Demonstrates how to catch all exceptions
catch_all() ->
	
	try 
		unsafe(throw,"Catch All")
	catch
		_:_ -> io:format("Catch All~n")
	end.


%% Demonstrates catching different exceptions and different types of exceptions
catch_me(Type) when is_atom(Type) ->
	try 
		unsafe(Type,"The Reason")
	catch
		oops -> got_throw_oops;
		throw:Message -> {got_throw, Message};
		exit:Message -> {got_exit, Message};
		error:Message -> {got_error,Message}
	end.
	
%% Demonstrate the use of the 'try - of' construct
tryof(Val) ->
	try
		unsafe(Val,"Some Message") 
	of
		%% This should be like a 'case of' 
		pass -> io:format("Pass ~n");
		fail -> io:format("FAIL ~n");
		_ -> io:format("Other: ~n")
	catch
		_:_ -> io:format("The Error: ~n")
	end.

%% Demonstrate the use of 'after' in a try - catch clause
after_clause() ->

	{ok,FileHandle} = file:open("/tmp/fred.txt", [read,write]),
	try 
		write_file(FileHandle,"line1~nline2~nline3~n"),
		Text = read_file(FileHandle),
		io:format(Text)
	%% Obviously add catch clause in here if required

	%% This isjust like Java finally
	after
		file:close(FileHandle)
	end.

%% Demonstrates getting a stack trace
stack(Type) when is_atom(Type) ->
	try 
		unsafe(Type,"The Reason")
	catch
		%%  Catch anything and return the stack trace
		_:_ -> erlang:get_stacktrace()
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% Illstrate the three types of exceptions:
%%    throw(..) - as in a recoverable user exception
%%    exit(...) - exit the app; could be an error, could be a normal termination
%%    erlang:error(...) - an internal error (usually used when writing libraries)
unsafe(Type,Message) when is_atom(Type) ->

	case Type of
		oops -> throw(oops);
		throw -> throw(Message);
		exit -> exit(Message);
		erlang_error -> erlang:error(Message);
		_ -> Type
	end.

%% Read and display the contents of a file
read_file(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ read_file(Device)
    end.

%% Write some junk into the fie
write_file(FileHandle,Text) ->
	io:fwrite(FileHandle,Text,[]).


