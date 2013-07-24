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
%% @doc Starts the server  @end
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
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
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
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(Msg, State) ->
    {noreply, State}.


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
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


