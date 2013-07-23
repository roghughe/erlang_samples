%% @author Roger
%% @doc 'fun' examples


-module(funs).

%% ====================================================================
%% API functions
%% ====================================================================
-export([anon_fun/0,anon_fun2/0,anon_fun3/0,yes_no/3,local_fun/0,remote_module_fun/0,render/2,to_html/1]).

%% @doc An anonymous fun
anon_fun() ->
	
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

	yes_no(Anonymous,false,false).
	
%% @doc Same anonymouse example using logic to replace the case
anon_fun2() ->
	Anon2 = fun(C,D) -> C or D end,
	
	yes_no(Anon2,true,false).
	
%% @doc Same anonymous example inlined, without a variable
anon_fun3() ->
	yes_no(fun(C,D) -> C or D end,false,true).

	
%% @doc Local funs
local_fun() ->
	
	Local = fun failing_or/2,
	yes_no(Local,false,false).

%% Remote module funs
remote_module_fun() ->
	Funs = fun clause_case:either_or_both1/2,
	yes_no(Funs,true,false).



%% Evaluate the 'Funs' function to output yes or no.
yes_no(Funs,A,B) ->
	case Funs(A,B) of 
		true -> io:format("yes~n");
		false -> io:format("no~n")
	end.

%%% Closure example - it's about scope...
%%% A varible defined in a function is carried over to a fun definition

%% @doc Example: Render some HTML
%% The 'Text' vars value is avaible to he anonymous fun when it's called in the to_html/1 function.
render(Text,Em) ->
	Out = to_html(fun() ->
					"<" ++ Em ++ ">" ++ Text ++ "</" ++ Em ++ ">"
				  end),
	io:fwrite(Out).

%% @doc  Add p tags to some text
to_html(EmphasisFun) ->
	"<p>" ++ EmphasisFun() ++ "</p>~n".


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc  An 'or' method that fails by always returning true
failing_or(_A,_B) ->
	true.



