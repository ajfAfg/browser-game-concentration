-module(to_string_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/src/test.hrl").
-include_lib("src/trump_card/trump.hrl").

return_string_test_() ->
	{?TEST_NAME,
	[return_string_of_suit_comma_number(),
	 error_if_the_argument_type_is_not_trump()]}.

return_string_of_suit_comma_number() ->
	T = trump:trump(spade, 1),
	[?_assertEqual(trump:to_string(T), "spade,1")].

error_if_the_argument_type_is_not_trump() ->
	[?_assertError(function_clause, trump:to_string(nil) )].
