-module(is_suit_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/src/test.hrl").

return_boolean_test_() ->
	{?TEST_NAME,
	 [return_true_if_the_argument_is_spade_club_diamond_or_heart(),
	 return_false_if_the_argument_is_other()]}.

return_true_if_the_argument_is_spade_club_diamond_or_heart() ->
	Suits = [spade, club, diamond, heart],
	[?_assert(suit:is_suit(Suit)) || Suit <- Suits].

return_false_if_the_argument_is_other() ->
	[?_assertNot(suit:is_suit(V)) || V <- [nil]].
