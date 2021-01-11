-module(trump_number).

-export([is_trump_number/1]).

-export_type([trump_number/0]).

-type trump_number() :: 1..13.

-spec is_trump_number(Term :: term()) -> boolean().
is_trump_number(Term) when is_integer(Term), 1 =< Term, Term =< 13 ->
	true;
is_trump_number(_Term) ->
	false.
