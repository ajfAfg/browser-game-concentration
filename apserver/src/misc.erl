-module(misc).

-export([shuffle/1]).

shuffle(List) when is_list(List) ->
	[Y || {_,Y} <- lists:sort([ {rand:uniform(), N} || N <- List])].
