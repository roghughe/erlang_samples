%% @author Roger
%% @doc Some example ETS code - data tables shared between processes. 
%% ETS tables can be accesses by multiple processes
%% ...hence the code


-module(ets_sample).

%% ====================================================================
%% API functions
%% ====================================================================
-export([myproc/4,create/0]).

%% Create an ETS table and insert some data 
create() ->
	Table = ets:new(contacts,[set,named_table]),
	ets:insert(Table,{"Roger",roghughe@captaindebug.com,"555 1234"}),
	
	spawn(?MODULE,myproc,[Table,"Roger",self(),10]),
	receive
		_ -> print(Table),
			 delete_table(Table)
	end.


%% Proc that reads the table
myproc(_,_,Owner,0) -> 
	timer:sleep(100),
	%% Send a message to the owning process to delte the table
	Owner ! [],
	timer:sleep(100),
	io:fwrite("About to exit...~n"),
	%% Kill all the linked processes
	exit(delete_table);
myproc(Table,User,Owner,N) ->
	% create the next point
	spawn_link(fun() -> myproc(Table,User,Owner,N-1) end),
	print(Table,User).


%% ====================================================================
%% Internal functions
%% ====================================================================


%% The table delete has to be done by the owning process
delete_table(Table) ->
	ets:delete(Table),
	io:fwrite("Table Deleted~n").

%% Read a table and print it's info
print(Table) ->
	Info = ets:info(Table),
	io:format("Info: ~p~n",[Info]).

%% Read a table and print the contents
print(Table,User) ->
	[{_,Email,Phone}] = ets:lookup(Table,User),
	io:format("User: ~p, Contact Email ~p Phone: ~p~n",[User,Email,Phone]).







