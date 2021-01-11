-module(is_trump_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/src/test.hrl").
-include_lib("src/trump_card/trump.hrl").

return_boolean_test_() ->
	{?TEST_NAME,
	 [return_true_if_the_argument_has_suit_and_trump_number(),
	  return_false_if_the_argument_has_others()]}.

return_true_if_the_argument_has_suit_and_trump_number() ->
	Suits = [spade, club, diamond, heart],
	Nums = lists:seq(1, 13),
	Trumps = [#trump{suit=Suit,number=Num} || Suit <- Suits, Num <- Nums],
	[?_assert(trump:is_trump(Trump)) || Trump <- Trumps].

return_false_if_the_argument_has_others() ->
	[?_assertNot(trump:is_trump(T)) || T <- [nil,"trump"] ].
