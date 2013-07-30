%% @author Roger
%% @doc @todo Add description to simple_cache_element.


-module(simple_cache_element).
-behaviour(gen_server).
-export([init/1,
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2,
		 create/2,
		 create/1,
		 fetch/1,
		 replace/2,
		 delete/1
		]).

-define(SERVER,?MODULE).
-define(DEFAULT_LEASE_TIME,60*60*24).


start_link(Value,LeaseTime) ->
	gen_server:start_link(?MODULE, [Value,LeaseTime], []).

create(Value,LeaseTime) ->
	simple_cache_sup:start_child(Value, LeaseTime).

create(Value) ->
	create(Value,?DEFAULT_LEASE_TIME).

fetch(Pid) ->
	gen_server:call(Pid,fetch).

replace(Pid,Value) ->
	gen_server:cast(Pid,{replace,Value}).

delete(Pid) ->
	gen_server:cast(Pid,delete).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {value,lease_time,start_time}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
%% The Big Idea of the init function is to setup the state record... that's all.
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Value,LeaseTime]) ->
	log4erl:info("simple_cache_element:init(~p,~p)",Value,LeaseTime),
	Now = calendar:local_time(),
	StartTime = calendar:datetime_to_gregorian_seconds(Now),

    {ok, #state{value=Value,
				lease_time=LeaseTime,
				start_time = StartTime},
	 time_left(StartTime,LeaseTime)}.

time_left(_StartTime,infinity) ->
	infinity;
time_left(StartTime,LeaseTime) ->
	Now = calendar:local_time(),
	CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
	TimeElapsed = CurrentTime-StartTime,
	case LeaseTime - TimeElapsed of
		Time when Time =< 0 -> 0;
		Time                -> Time * 1000
	end.



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
handle_call(fetch, _From, State) ->
	#state{value=Value,
		   lease_time = LeaseTime,
		   start_time=StartTime} = State,
	TimeLeft = time_left(StartTime,LeaseTime),
    {reply, {ok,Value}, State,TimeLeft}.


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
handle_cast({replace,Value}, State) ->
	% Get the various bits out of the State tuple
	#state{lease_time = LeaseTime,
		   start_time = StartTime} = State,
	TimeLeft = time_left(StartTime,LeaseTime),
    {noreply, {ok,Value}, State,TimeLeft};
handle_cast(delete,State) ->
	{stop,normal,State}.



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
handle_info(timeout, State) ->
	{stop,normal,State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    simple_cache_store:delete(self()),
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


