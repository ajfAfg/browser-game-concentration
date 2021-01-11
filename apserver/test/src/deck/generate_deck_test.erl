-module(generate_deck_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/src/test.hrl").
-include_lib("src/trump_card/trump.hrl").

return_list_that_contains_trumps_test_() ->
	{?TEST_NAME,
	 [all_elements_of_list_are_trump(),
	  list_has_no_duplicate_trumps(),
	  list_length_is_fifty_two()]}.

all_elements_of_list_are_trump() ->
   	Deck = deck:generate_deck(),
	[?_assert(lists:all(fun trump:is_trump/1,Deck) )].

list_has_no_duplicate_trumps() ->
	Deck = deck:generate_deck(),
	Uset = ordsets:from_list(Deck),
	[?_assertEqual(length(Deck),length(Uset) )].

list_length_is_fifty_two() ->
	Deck = deck:generate_deck(),
	[?_assertEqual(length(Deck),52)].
