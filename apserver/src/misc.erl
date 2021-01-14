-module(misc).

-export([shuffle/1]).
-export([len/1]).
-export([get_entity_with_tag/2]).

-spec shuffle(List :: list()) ->list().
shuffle(List) when is_list(List) ->
	[Y || {_,Y} <- lists:sort([ {rand:uniform(), N} || N <- List])].

-spec len(String :: string()) -> non_neg_integer().
len(String) when is_list(String) ->
	integer_to_list(length(String) ).

%% HACK: This function does not fit in here. Need modeling Entity and Entity list.
get_entity_with_tag(EntityList, Tag) ->
	case lists:keyfind(Tag, 1, EntityList) of
		{_, Entity} ->
			Entity;
		false ->
			""
	end.
%	{_, Entity} = lists:keyfind(Tag, 1, EntityList),
%	Entity.
