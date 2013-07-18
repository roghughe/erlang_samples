%% @author Roger
%% @doc @todo Add description to funs.


-module(funs).

%% ====================================================================
%% API functions
%% ====================================================================
-export([demo_funs/0,yes_no/3]).

%% Demo all the different cases using funs and the clause_case module
demo_funs() ->
	
	% Anonymous fun 
	Anonymous = fun (A,B) ->
				% Make a tuple from the args so you can match...
				case{A,B} of 
					{true,B} when is_boolean(B) ->
						true;
					{A,true} when is_boolean(A) ->
						true;
					{false,false} ->
						false
				end
				end,

	yes_no(Anonymous,false,false),
	
	% Local funs
	Local = fun failing_or/2,
	yes_no(Local,false,false),
	
	% Remote module funs
	Funs = fun clause_case:either_or_both1/2,
	yes_no(Funs,true,false).

% Evaluate the 'Funs' function to output yes or no.
yes_no(Funs,A,B) ->
	case Funs(A,B) of 
		true -> io:format("yes~n");
		false -> io:format("no~n")
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

% An 'or' method that fails by always returning true
failing_or(A,B) ->
	true.



