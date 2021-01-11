-module(trump_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("test/src/test.hrl").
-include_lib("src/trump_card/trump.hrl").

return_record_that_has_atom_and_integer_test_() ->
	{?TEST_NAME,
	 [atom_is_spade_club_diamond_or_heart(),
	  integer_is_one_to_twelve(),
	  throw_exception_if_the_arguments_are_invalid()]}.

atom_is_spade_club_diamond_or_heart() ->
	Atoms = [spade, club, diamond, heart],
	T1s = [trump:trump(Suit,1) || Suit <- Atoms],
	T2s = [#trump{suit=Suit, number=1} || Suit <- Atoms],
	[?_assertEqual(T1,T2) || {T1,T2} <- lists:zip(T1s,T2s)].

integer_is_one_to_twelve() ->
	Nums = lists:seq(1, 13),
	T1s = [trump:trump(spade,N) || N <- Nums],
	T2s = [#trump{suit=spade,number=N} || N <- Nums],
	[?_assertEqual(T1,T2) || {T1,T2} <- lists:zip(T1s,T2s)].

throw_exception_if_the_arguments_are_invalid() ->
	[?_assertThrow(badarg, trump:trump(bad,1) ),
	 ?_assertThrow(badarg, trump:trump("spade",1) ),
	 ?_assertThrow(badarg, trump:trump(spade,0) ),
	 ?_assertThrow(badarg, trump:trump(spade,14) ),
	 ?_assertThrow(badarg, trump:trump(spade,1.1) )].
