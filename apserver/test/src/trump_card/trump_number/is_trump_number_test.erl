-module(is_trump_number_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/src/test.hrl").

return_boolean_test_() ->
	{?TEST_NAME,
	 [return_true_if_the_argument_is_one_to_twelve(),
	 return_false_if_the_argument_is_other()]}.

return_true_if_the_argument_is_one_to_twelve() ->
	Nums = lists:seq(1, 13),
	[?_assert(trump_number:is_trump_number(Num)) || Num <- Nums].

return_false_if_the_argument_is_other() ->
	[?_assertNot(trump_number:is_trump_number(V)) || V <- [0,14,nil]].
