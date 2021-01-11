-module(suit).

-export([is_suit/1]).

-export_type([suit/0]).

-type suit() :: spade | club | diamond | heart.

-spec is_suit(Term :: term()) -> boolean().
is_suit(Term) when Term=:=spade orelse Term=:=club orelse Term=:=diamond orelse Term=:=heart ->
	true;
is_suit(_Term) ->
	false.
