%% @author Roger
%% @doc @todo Add description to simple_cache_element_tests.

-include_lib("eunit/include/eunit.hrl").

-module(simple_cache_element_tests).

-define(RETURN,{ok,2500}).
-define(VALUE,"value").
-define(LEASETIME,3000).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% @doc Example of how to use meck (this isn't the best way see: <a href="http://labs.mochimedia.com/archive/2011/06/13/meck-eunit-best-practices/">meck and eunit best practices</a>)
%% Note:  this throws an exception as gen_server is a sticky module (i.e. a module located in a directory that's marked as sticky and cannot be reloaded)
start_link_test() ->
    try
	    meck:new(gen_server),
    	meck:expect(gen_server, % module
				start_link,		% function
				3,				% number of arguments
				?RETURN			% expected return value 
			   ),
        ?assertEqual(?RETURN,simple_cache_element:start_link(?VALUE, ?LEASETIME))
	catch
		_:_ -> io:format("Expected Failure"),
		ok
    after
        meck:validate(gen_server),
        meck:unload(gen_server)
    end.



init_test() ->
	simple_cache_element:init([?VALUE,?LEASETIME]).
	


%% ====================================================================
%% Internal functions
%% ====================================================================


