-module(deck).

-export([generate_deck/0]).
-export([convert_csv/1]).

-export_type([deck/0]).

-type deck() :: list(trump() ).

-include_lib("src/trump_card/trump.hrl").

-spec generate_deck() -> deck().
generate_deck() ->
	Suits = [spade, club, diamond, heart],
	Nums = lists:seq(1, 13),
	[trump:trump(Suit,Num) || Suit <- Suits, Num <- Nums].

-spec convert_csv(Deck :: deck()) -> string().
convert_csv(Deck) ->
	"ok".
