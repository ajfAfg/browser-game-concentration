-module(shuffle_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/src/test.hrl").

return_list_test_() ->
	{?TEST_NAME,
	 [return_list_shuffled(),
	  error_if_the_argument_type_is_not_list()]}.

return_list_shuffled() ->
	L1 = [0],
	L2 = lists:seq(1, 100),
	L3 = lists:zip(L2, L2),
	[?_assertEqual(L1, misc:shuffle(L1) ),
	 ?_assertNotEqual(L2, misc:shuffle(L2) ),
	 ?_assertNotEqual(L3, misc:shuffle(L3) )].

error_if_the_argument_type_is_not_list() ->
	[?_assertError(function_clause, misc:shuffle(nil) ),
	 ?_assertError(function_clause, misc:shuffle(1) )].
