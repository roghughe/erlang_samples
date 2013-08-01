%% @author Roger
%% @doc Simple server that starts up and then times out and closes down. On close down throws and exception


-module(temp_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER,?MODULE).
-define(SLEEP_TIME,1000). % sleep time in mille secnds

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).


%% @doc Start the Server
%% Running this server produces the following error on timeout:
%%
%%=ERROR REPORT==== 1-Aug-2013::16:50:52 ===
%%** Generic server temp_server terminating 
%%** Last message in was timeout
%%** When Server state == {state}
%%** Reason for termination == 
%%** {{badmatch,right_now},
%%    [{temp_server,handle_info,2,[{file,"src/temp_server.erl"},{line,91}]},
%%     {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,604}]},
%%     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,239}]}]}
%%** exception error: no match of right hand side value right_now
%%     in function  temp_server:handle_info/2 (src/temp_server.erl, line 91)
%%     in call from gen_server:handle_msg/5 (gen_server.erl, line 604)
%%     in call from proc_lib:init_p_do_apply/3 (proc_lib.erl, line 239)
%%
%%  If you type <tt>application:start(sasl).</tt> into the shell, ypu get the following:
%%=ERROR REPORT==== 1-Aug-2013::16:51:18 ===
%%** Generic server temp_server terminating 
%%** Last message in was timeout
%%** When Server state == {state}
%%** Reason for termination == 
%%** {{badmatch,right_now},
%%    [{temp_server,handle_info,2,[{file,"src/temp_server.erl"},{line,91}]},
%%     {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,604}]},
%%     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,239}]}]}
%%   
%%=CRASH REPORT==== 1-Aug-2013::16:51:18 ===
%%  crasher:
%%    initial call: temp_server:init/1
%%    pid: <0.47.0>
%%    registered_name: temp_server
%%    exception exit: {{badmatch,right_now},
%%                     [{temp_server,handle_info,2,
%%                                   [{file,"src/temp_server.erl"},{line,91}]},
%%                      {gen_server,handle_msg,5,
%%                                  [{file,"gen_server.erl"},{line,604}]},
%%                      {proc_lib,init_p_do_apply,3,
%%                                [{file,"proc_lib.erl"},{line,239}]}]}
%%      in function  gen_server:terminate/6 (gen_server.erl, line 744)
%%    ancestors: [<0.36.0>]
%%    messages: []
%%    links: [<0.36.0>]
%%    dictionary: []
%%    trap_exit: false
%%    status: running
%%    heap_size: 376
%%    stack_size: 27
%%    reductions: 134
%%  neighbours:
%%    neighbour: [{pid,<0.36.0>},
%%                  {registered_name,[]},
%%                  {initial_call,{erlang,apply,2}},
%%                  {current_function,{shell,eval_loop,3}},
%%                  {ancestors,[]},
%%                  {messages,[]},
%%                  {links,[<0.26.0>,<0.47.0>]},
%%                  {dictionary,[]},
%%                  {trap_exit,false},
%%                  {status,waiting},
%%                  {heap_size,376},
%%                  {stack_size,7},
%%                  {reductions,1169}]
%%** exception error: no match of right hand side value right_now
%%     in function  temp_server:handle_info/2 (src/temp_server.erl, line 91)
%%     in call from gen_server:handle_msg/5 (gen_server.erl, line 604)
%%     in call from proc_lib:init_p_do_apply/3 (proc_lib.erl, line 239)
%% @end
start_link() ->
	gen_server:start_link({local,?SERVER},?MODULE,[],[]).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

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
    {ok, #state{},?SLEEP_TIME}.


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
handle_call(_Request, _From, State) ->
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
handle_cast(_Msg, State) ->
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
handle_info(_Info, State) ->
	i_want_to_die = right_now,	% This will fail when called
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
terminate(_Reason, _State) ->
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


