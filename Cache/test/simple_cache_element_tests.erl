%% @author Roger
%% @doc @todo Add description to simple_cache_element_tests.

-include_lib("eunit/include/eunit.hrl").

-module(simple_cache_element_tests).

-define(RETURN,{ok,2500}).
-define(VALUE,"value").
-define(LEASETIME,3000).
-define(STARTTIME,1000).
-define(TIMELEFT,0).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-define(ARGS,{dummy,start_args}).

%% @doc Example of how to use meck (this isn't the best way see: <a href="http://labs.mochimedia.com/archive/2011/06/13/meck-eunit-best-practices/">meck and eunit best practices</a>)
%% Note:  this throws an exception as gen_server is a sticky module (i.e. a module located in a directory that's marked as sticky and cannot be reloaded)
start_link() -> %% commented out...  _test() ->
    try
	    meck:new(gen_server),
    	meck:expect(gen_server, % module
				start_link,		% function
				3,				% number of arguments
				?RETURN			% expected return value 
			   ),
        ?assertEqual(?RETURN,simple_cache_element:start_link(?ARGS,?VALUE, ?LEASETIME))
	catch
		_:_ -> io:format("Expected Failure"),
		ok
    after
        meck:validate(gen_server),
        meck:unload(gen_server)
    end.


%% Returns the initial State of the gen_server.  
%% The state record: state, {value,lease_time,start_time}
init_test_() ->
	{ok,{state,Value,LeaseTime,_StartTime},TimeLeft} = simple_cache_element:init([?ARGS,?VALUE,?LEASETIME]),
	[?_assertEqual(?VALUE,Value),
     ?_assertEqual(3000,LeaseTime),
	 ?_assertEqual(3000000,TimeLeft)].


handle_call_for_fetch_test_() ->
	ExpectedState = {state,?VALUE,?LEASETIME,?STARTTIME},
	{reply, {ok,Value}, State,TimeLeft} = simple_cache_element:handle_call(fetch, from_unused, ExpectedState),
	[?_assertEqual(?VALUE,Value),
     ?_assertEqual(ExpectedState,State),
     ?_assertEqual(?TIMELEFT,TimeLeft)].


handle_cast_for_replace_test_() ->
	ExpectedState = {state,?VALUE,?LEASETIME,?STARTTIME},
	{noreply,  State,TimeLeft} = simple_cache_element:handle_cast({replace,?VALUE},ExpectedState),
	[?_assertEqual(ExpectedState,State),
	 ?_assertEqual(?TIMELEFT,TimeLeft)].

handle_cast_for_delete_test_() ->
	ExpectedState = {state,?VALUE,?LEASETIME,?STARTTIME},
	{stop, normal, State} = simple_cache_element:handle_cast(delete,ExpectedState),
	[?_assertEqual(ExpectedState,State)].




%% ====================================================================
%% Internal functions
%% ====================================================================


