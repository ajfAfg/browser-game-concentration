-module(get_number_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/src/test.hrl").
-include_lib("src/trump_card/trump.hrl").

return_integer_test_() ->
	{?TEST_NAME,
	[return_integer_which_trump_has(),
	 error_if_the_argument_type_is_not_trump()]}.

return_integer_which_trump_has() ->
	OriginalNums = lists:seq(1, 13),
	Ts = [trump:trump(spade,N) || N <- OriginalNums],
	TrumpNums = lists:map(fun trump:get_number/1, Ts),
	[?_assertEqual(O,T) || {O,T} <- lists:zip(OriginalNums,TrumpNums)].

error_if_the_argument_type_is_not_trump() ->
	[?_assertError(function_clause, trump:get_number(nil) )].
