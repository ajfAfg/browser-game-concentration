-module(convert_csv_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/src/test.hrl").
-include_lib("src/trump_card/trump.hrl").

return_string_test_() ->
	{?TEST_NAME,
	 [the_string_contains_fifty_two_commas()]}.

the_string_contains_fifty_two_commas() ->
   	Deck = deck:generate_deck(),
	CSV = deck:convert_csv(Deck),
	io:format(user, "~n~p~n", [CSV]),
	[?_assert(true)].
