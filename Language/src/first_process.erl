%% @author Roger
%% @doc Sample code demostrating a 'my first process'


-module(first_process).

%% ====================================================================
%% This is a first set of examples using spawn
%% ====================================================================
-export([run/0,out/0,out2/0,print_this/1,hello_process/0,do_link1/0,do_link2/0,chain/1]).

%% Spawn a ping/0 process and send it a message
run() ->
	io:fwrite("Ping~n"),
	Pid = spawn(fun ping/0),
	
	% Send a message to Pid using our own pid as the data - the '!' char is used to send messages
	Pid ! self(),
	receive
		pong -> 
			% code run when a 'pong' is received
			io:fwrite("Pong~n"),
			ok
	end.

ping() ->
	receive
		% This is the Sender's PID
		From -> 
			% reply with a pong atom
			From ! pong
	end.



%% Example of sending data to a process
out() ->
	io:fwrite("Printing~n"),
	Pid = spawn(fun print/0),
	
	Pid ! {message,is,a,tuple},
	sent.

print() ->
	receive
		Message -> 
			io:format("The message is: ~p~n",[Message])
	end.




%% Example of sending data to a process using a differnt spawn function
out2() ->
	io:fwrite("Printing2~n"),
	
	% In this case the print_this function has to be exported
	% Args MUST be added to a list, but they are separated for the call.
	Pid = spawn(?MODULE,print_this,[the_arg]),
	
	Pid ! "This is my message",
	sent.

%% Another print method - prints a message. This method must be exported to be called using the out2 version of spawn
print_this(Arg) ->
	receive
		Message -> 
			io:format("The message is: ~p~nThe Arg is: ~p~n",[Message,Arg])
	end.



%% Create a process that calls hello:hello_world()
hello_process() ->
	
	% In this case the print_this function has to be exported
	Pid = spawn(hello,hello_world,[]),
	% Send an empty message
	Pid ! [],
	sent.



%% Example for spawn_link -- myproc exits before this func has time to complete
do_link1() ->
	spawn_link(fun myproc/0),
	timer:sleep(1000),
	io:fwrite("On No! (This won't get printed)~n").
	
%% A proc that waits and exits
myproc() ->
	timer:sleep(500),
	exit('Done').  % Note the apostrophies around the atom owing to the capital D



%% Spawn an link many processes - all of which will die
do_link2() ->
	spawn(fun start_chain/0),
	ok.

%% Start the chanin of processes. This extra step is here so
start_chain() ->
	process_flag(trap_exit, true),
	spawn(?MODULE, chain, [10]),
	timer:sleep(1000).


%% Chains lots of do nothing processes together
chain(0) ->
	io:format("Chain 0~n"),
	receive
		_ -> ok
	after 500 ->
			io:fwrite("Killing the chain"),
			exit("chain dies here")
	end;
chain(N) ->
	io:format("Chain ~w~n",[N]),
	Pid = spawn(fun() -> chain(N-1) end),
	link(Pid),
	receive
		_ -> ok
	end.

