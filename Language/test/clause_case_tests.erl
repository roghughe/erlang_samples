%% @author Roger
%% @doc Test sample 'clause' and 'case' examples, which functionally do the same thing.


-module(clause_case_tests).

-include_lib("eunit/include/eunit.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

either_or_both1_test_() ->
	[?_assert(clause_case:either_or_both1(true, true)),
	 ?_assert(clause_case:either_or_both1(true, false)),
	 ?_assert(clause_case:either_or_both1(false, true)),
	 ?_assertNot(clause_case:either_or_both1(false, false))].


either_or_both2_test_() ->
	[?_assert(clause_case:either_or_both2(true, true)),
	 ?_assert(clause_case:either_or_both2(true, false)),
	 ?_assert(clause_case:either_or_both2(false, true)),
	 ?_assertNot(clause_case:either_or_both2(false, false))].


%% ====================================================================
%% Internal functions
%% ====================================================================


