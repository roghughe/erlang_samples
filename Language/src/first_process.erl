%% @author Roger
%% @doc Sample code demostrating a 'my first process'


-module(first_process).

%% ====================================================================
%% This is a first set of examples using spawn
%% ====================================================================
-export([run/0,out/0,out2/0,print_this/1,hello_process/0,do_link1/0,do_link2/0,chain/1,monitor_you/0,
		 my_registered/0,is_init_process_running/0,is_init_process_running2/0,my_register/0]).

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

%% An example of a monitor. A monitor gets a message when a child process dies, but doesn't die itself
%% @see http://www.erlang.org/doc/man/erlang.html#monitor-2
%% Also demonstrates message selection using pattern matching
monitor_you() ->
	Pid = spawn(fun myproc/0),
	Ref = monitor(process,Pid),
	receive
		% Match the incoming message to a pattern - 
		% this tuple is the message sent when the myproc process dies.
		{'DOWN', MessageRef, process, _Object, _Info} when MessageRef =:= Ref -> Ref;
		
		% Pattern2 when Guard2 -> body2;
		% :
		% PatternN when GuardN -> bodyN;
		
		% Message doesn't match any patterns, so remove it and exit (optional, obviously)
		Default -> Default
	end.


%% Get hold of a list of all the requstered processes
my_registered() ->
	registered().


%% Return true if the system process 'init' is running
%% There is no list.contains(value) in erlang, so filtre the list and check the size of the result
%% Should return true as the init process is a system process that's usually running
is_init_process_running() ->
	Processes = registered(),
	Pred = fun(X) -> X =:= init end,
	Length = length(lists:filter(Pred, Processes)),
	if 
		Length == 0 -> false;
		Length == 1 -> true;
		Length > 1 -> throw({oops,"Muliple init processes"})
	end.
		 
%% Return true if the system process 'init' is running
%% There is no list.contains(value) in erlang, so use a list comprehension to parse the list lookinh for 'init'
%% Should return true as the init process is a system process that's usually running
is_init_process_running2() ->
	
	Result = [ X || X <- registered(), X =:= init],
	
	Length = length(Result),
	if 
		Length == 0 -> false;
		Length == 1 -> true;
		Length > 1 -> throw({oops,"Muliple init processes"})
	end.
		 
%% A generic is_process running function
is_process_running(Process) ->
	Result = [ X || X <- registered(), X =:= Process],
	
	Length = length(Result),
	if 
		Length == 0 -> false;
		Length == 1 -> true;
		Length > 1 -> throw({oops,"Muliple init processes"})
	end.



%% Register a process (it should appear in the list of registered processes afterwards)
my_register() ->

	Process = my_proc_name,
	R = is_process_running(Process),
	if 
		R == true -> throw(already_running_ooops);
		R == false -> io:fwrite("Not running - okay~n")
	end,
	
	Pid = spawn(fun myproc2/0),
	register(Process,Pid),
	
	R2 = is_process_running(Process),
	if 
		R2 == true -> io:fwrite("Process is running~n");
		R2 == false -> throw(not_running)
	end,
	R2.


%% Another proc that waits and exits
myproc2() ->
	timer:sleep(500),
	exit('Done').  % Note the apostrophies around the atom owing to the capital D


