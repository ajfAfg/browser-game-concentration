-module(list_to_csv_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/src/test.hrl").

return_string_test_() ->
	{?TEST_NAME,
	 [string_format_is_csv(),
	  throw_exception_if_the_argument_type_is_not_list(),
	  throw_exception_if_the_list_has_no_element(),
	  error_if_the_list_does_not_have_type_list_only()]}.
	
string_format_is_csv() ->
	CSV1 = csv:list_to_csv([ [1,2] ]),
	CSV2 = csv:list_to_csv([ [1,2], [3,4] ]),
	CSV3 = csv:list_to_csv([ [1,2], [3,nil] ]),
	[?_assertEqual(CSV1, "1,2\r\n"),
	 ?_assertEqual(CSV2, "1,2\r\n3,4\r\n"),
	 ?_assertEqual(CSV3, "1,2\r\n3,nil\r\n")].

throw_exception_if_the_argument_type_is_not_list() ->
	[?_assertThrow(badarg, csv:list_to_csv(nil) )].

throw_exception_if_the_list_has_no_element() ->
	[?_assertThrow(badarg, csv:list_to_csv([]) )].

error_if_the_list_does_not_have_type_list_only() ->
	[?_assertError(badarg, csv:list_to_csv([nil]) )].
