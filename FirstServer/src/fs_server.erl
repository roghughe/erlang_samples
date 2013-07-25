%% ====================================================================
%% @author Roger
%%  [http://www.captaindebug.com]
%% @doc Very simple first server implementation that does RPC over TCP. Largely taken 
%% from Manning's Erlang and OTP in Action book by Eric Merrit and Richard Carlsson.
%% @end
%% ====================================================================


-module(fs_server).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER,?MODULE).
-define(DEFAULT_PORT,1055).
	
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,start_link/0,get_count/0,stop/0]).

%% ====================================================================
%% @doc Starts the server  
%% @spec start_link(Port::integer()) -> {ok,Pid}
%% where 
%%  Pid = pid()
%% @end
%% ====================================================================
start_link(Port) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Port],[]).


%% @spec start_link() -> {ok,Pid}
%% @doc Calls start_link/1 using the default port. 
start_link() ->
	log4erl:info("Starting the Server"),	
	start_link(?DEFAULT_PORT).


%% ====================================================================
%% @doc Fetches the number of requests made to this server internally
%% using the synchronous call to gen_server:call/2
%% @spec get_count() -> {ok, Count}
%%   where 
%%    Count = integer()
%% @end
%% ====================================================================
get_count() ->
	gen_server:call(?SERVER, get_count).

%% ====================================================================
%% @doc Stops the server internally using an asynchronous call to 
%% gen_server:cast/2
%% @spec stop() -> ok
%% @end
%% ====================================================================
stop() ->
	log4erl:info("Stopping the Server"),	
	gen_server:cast(?SERVER, stop).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {port,lsock,request_count = 0}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Port]) ->
	log4erl:info("In Init..."),	
	{ok,LSock} = gen_tcp:listen(Port, [{active,true}]),
    {ok, #state{port = Port,lsock=LSock},0}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
%% Note that this version of handle_call() only accepts 'get_count' calls. Use clauses to add in more argument types.


-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(get_count, _From, State) ->
	Count = State#state.request_count,
	log4erl:info("Calling get_count, which is ~p~n",[Count]),	
    Reply = {ok,Count},
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
%% Handle stop messages only
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(stop, State) ->
    {stop,normal, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({tcp,Socket,RawData},State) ->
	log4erl:info("RawData is ~p~n",[RawData]),	
	do_rpc(Socket,RawData),
	RequestCount = State#state.request_count,
	{noreply,State#state{request_count = RequestCount+1}};
handle_info(timeout,#state{lsock = LSock} = State) ->
	disk_log:log(?SERVER,"Waiting for client"),
	{ok,_Sock} = gen_tcp:accept(LSock),
	{noreply,State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, __State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

do_rpc(Socket,RawData) ->
	try
		{M,F,A} = split_out_mfa(RawData),
		Result = apply(M,F,A),
		gen_tcp:send(Socket,io_lib:fwrite("~p~n",[Result]))
	catch
		_Class:Err ->
			gen_tcp:send(Socket,io_lib:fwrite("~p~n",[Err]))
	end.

%% This is an example of how you do regex stuff in erlang...
split_out_mfa(RawData) ->
	MFA = re:replace(RawData, "\r\n$", "", [{return,list}]),
	{match,[Module,Function,Args]} = re:run(MFA,"(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
							 [{capture,[1,2,3],list},ungreedy]),
	{list_to_atom(Module),list_to_atom(Function),args_to_terms(Args)}.
							

args_to_terms(RawArgs) ->
	{ok,Toks,_Line} = erl_scan:string("[" ++ RawArgs ++ "]. ",1),
	{ok,Args} = erl_parse:parse_term(Toks),
	Args.
