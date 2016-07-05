%% @author Roger
%% @doc 'fun' examples


-module(funs).

%% ====================================================================
%% API functions
%% ====================================================================
-export([anon_fun/0,anon_fun2/0,anon_fun3/0,yes_no/3,local_fun/0,remote_module_fun/0,render/2,to_html/1,demo_alarm/0,
	map/2, incr/1,sample_map_call/0,filter/2,sample_filter/0,fold/3,sum/0,max/0]).

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
	
%% @doc Same anonymous example using logic to replace the case
anon_fun2() ->
	Anon2 = fun(C,D) -> C or D end,
	
	yes_no(Anon2,true,false).
	
%% @doc Same anonymous example inlined, without a variable
anon_fun3() ->
	yes_no(fun(C,D) -> C or D end,false,true).

	
%% @doc Local funs - This references a function in the same module
local_fun() ->
	
	Local = fun failing_or/2,
	yes_no(Local,false,false).

%% Remote module funs - This references a function in another module.
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
%%% A variable defined in a function is carried over to a fun definition

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

%%% Another Closure example from the erlang learn you some book.
demo_alarm() ->

	PrepareAlarm = fun(Room) ->
		io:format("Alarm set in room: ~s~n",[Room]),
		% Return a function...
		fun()-> io:format("Alarm tripped in Room ~s",[Room])  end
		end,

	AlarmReady = PrepareAlarm("kitchen"),

  AlarmReady().

%% ====================================================================
%%  These are examples of how to write generic functions - like Java
%%  8 has got (and what this has probably had for longer)
%% ====================================================================

%% map examples
map(_,[]) -> [];
map(Function,[Head | Tail]) -> [Function(Head)|map(Function,Tail)].

%% Sample functions
incr(X) -> X + 1.
%decr(X) -> X - 1.

sample_map_call() ->
	List = [1,2,3,4,5],
	map(fun incr/1, List).

% filter example using a Predicate
filter(Pred,List) -> lists:reverse(filter(Pred,List,[])).

filter(_,[],Acc) -> Acc;
filter(Pred,[Head | Tail],Acc) ->
	case Pred(Head) of
		true -> filter(Pred, Tail,[Head|Acc]);
		false -> filter(Pred, Tail,Acc)
	end.

sample_filter() ->
	List = [0,1,2,3,4,5],
	filter(fun(X) -> X rem 2 == 0 end,List).

%% Fold function example
fold(_,Start,[]) -> Start;
fold(Fold,Start,[Head | Tail]) -> fold(Fold, Fold(Head,Start),Tail).

sum() ->
	List = [1,2,3],
	fold(fun(A,B) -> A + B end, 0,List).

max() ->
	List = [4,5,3],
	fold(fun(A,B) when A > B -> A; (_,B) -> B end,0,List).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc  An 'or' method that fails by always returning true
failing_or(_A,_B) ->
	true.



