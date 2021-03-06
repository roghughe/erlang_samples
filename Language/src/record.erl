%% @author Roger
%% @doc Module that demostrates record syntax and usage.


-module(record).

%% Import a record that's defined in a header file and therefore available to multiple modules by include directive
-include("../include/record.hrl").


%% Define a Record - visible to this module only.
%% The format is 
%% -record(recordName, {tuple="default value", field2, field3, etc"}).
-record(customer, {name="<anonyous>", email,phone}).


%% ====================================================================
%% API functions
%% ====================================================================
-export([create/0,
		 access/0,
		 update/0,
		 update2/0,
		 update3/0,
		 update4/0,
		 access_ext/0]).

%% @doc Creating a record - example
create() ->
	
	%% Unused - demo creating and empty record
	_Record = #customer{},
	
	%% Unused - demo partial record creation
	_Record2 = #customer{email="info@captaindug.com"},

	#customer{name="Roger Hughes", email="info@captaindebug.com", phone="555 1234"}.

%% @doc Accessing record fields
access() ->
	
	Record = #customer{name="Roger Hughes", email="info@captaindebug.com", phone="555 1234"},

	Name = Record#customer.name,
	Email = Record#customer.email,
	Phone = Record#customer.phone,
	{Name,Email,Phone}.

%% @doc Demo the updating of a record by creating a new record (Single Assignment AKA immutability in Java)
update() ->
	Record = #customer{name="Roger Hughes", email="info@captaindebug.com", phone="555 1234"},

	#customer{name=Record#customer.name, email=Record#customer.email, phone="555 0987"}.

%% @doc Update a record as part of passing an arg to another function
update2() ->
	
	Record = #customer{name="Roger Hughes", email="info@captaindebug.com", phone="555 1234"},

	show_phone(Record),
	contact_result(Record).


%% @doc Update a record as part of passing an arg to another function
%% Throw an exception as the phone number is missing
update3() ->	
	%% Incomplete - missing phone number
	Record = #customer{name="Roger Hughes"},

	contact_result(Record).


%% @doc Better example of update3/0 - adds an exception in to handle the error as this should be recoverable
update4() ->
	
	try 
		%% Incomplete - missing phone number
		Record = #customer{name="Roger Hughes"},
		
		show_phone(Record)
	catch 
		_:_ -> io:fwrite("Missing Phone Number~n"),
		missing
	end.

%% @doc Accessing record fields defined in a header file
access_ext() ->
	
	Address = #address{line1 = "23 Railway Cuttings", town="East Cheam"},

	Street = Address#address.line1,
	Town = Address#address.town,
	{Street,Town}.



%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc display cotact details
%% Creates a new record as the arg is passed into the function
%% email field is ignored as it's not used
show_phone(#customer{name=Name, phone=Phone}) when Phone =/= undefined ->
	io:format("Contact: ~s, on ~s~n",[Name,Phone]).

contact_result(#customer{name=Name, phone=Phone}) ->
	{Name,Phone}.
