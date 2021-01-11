-module(room_server_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/src/test.hrl").

return_list_that_contains_trumps_test_() ->
	{?TEST_NAME,
	 {setup,
	  fun room_server:start_link/0,
	  [when_server_is_accessed_from_two_client()]
	 }}.

when_server_is_accessed_from_two_client() ->
	[?_assertEqual(ok,ok)].
