-module(trump).

-export([trump/2]).
-export([get_suit/1]).
-export([get_number/1]).
-export([is_trump/1]).
-export([to_string/1]).

-include_lib("src/trump_card/trump.hrl").

-spec trump(Suit :: suit:suit(), Num :: trump_number:trump_number()) -> trump().
trump(Suit, Num) ->
	case suit:is_suit(Suit) andalso trump_number:is_trump_number(Num) of
		true ->
			#trump{suit=Suit, number=Num};
		false ->
			throw(badarg)
	end.

-spec get_suit(Trump :: trump()) -> suit:suit().
get_suit(#trump{suit=Suit}) ->
	Suit.

-spec get_number(Trump :: trump()) -> trump_number:trump_number().
get_number(#trump{number=Num}) ->
	Num.

-spec is_trump(Trump :: trump()) -> boolean().
is_trump(#trump{suit=Suit, number=Num}) ->
	suit:is_suit(Suit) andalso trump_number:is_trump_number(Num);
is_trump(_Other) ->
	false.

-spec to_string(Trump :: trump()) -> string().
to_string(#trump{suit=Suit, number=Num}) ->
	lists:flatten(io_lib:format("~p,~p", [Suit, Num])).
