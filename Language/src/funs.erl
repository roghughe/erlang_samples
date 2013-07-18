%% @author Roger
%% @doc @todo Add description to funs.


-module(funs).

%% ====================================================================
%% API functions
%% ====================================================================
-export([demo_funs/0,yes_no/3,render/2]).

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
	
	% Same anonymouse example using logic to replace the case
	Anon2 = fun(C,D) -> C or D end,
	
	yes_no(Anon2,true,false),
	
	% Same anonymous example inlined, without a variable
	yes_no(fun(C,D) -> C or D end,false,true),

	
	
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

%%% Closure example - it's about scope...
%%% A varible defined in a function is carried over to a fun definition

%% Example: Render some HTML
%% The 'Text' vars value is avaible to he anonymous fun when it's called in the to_html/1 function.
render(Text,Em) ->
	Out = to_html(fun() ->
					"<" ++ Em ++ ">" ++ Text ++ "</" ++ Em ++ ">"
				  end),
	io:fwrite(Out).

%% Add <p> tags to some text
to_html(EmphasisFun) ->
	"<p>" ++ EmphasisFun() ++ "</p>~n".


%% ====================================================================
%% Internal functions
%% ====================================================================

% An 'or' method that fails by always returning true
failing_or(_A,_B) ->
	true.



