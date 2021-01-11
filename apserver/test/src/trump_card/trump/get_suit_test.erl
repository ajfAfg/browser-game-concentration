-module(get_suit_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/src/test.hrl").
-include_lib("src/trump_card/trump.hrl").

return_atom_test_() ->
	{?TEST_NAME,
	[return_atom_which_trump_has(),
	 error_if_the_argument_type_is_not_trump()]}.

return_atom_which_trump_has() ->
	Atoms = [spade, club, diamond, heart],
	Ts = [trump:trump(Suit,1) || Suit <- Atoms],
	Suits = lists:map(fun trump:get_suit/1, Ts),
	[?_assertEqual(A,S) || {A,S} <- lists:zip(Atoms,Suits)].

error_if_the_argument_type_is_not_trump() ->
	[?_assertError(function_clause, trump:get_suit(nil) )].
