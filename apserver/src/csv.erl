-module(csv).

-export([list_to_csv/1]).

-spec list_to_csv(list()) -> string().
list_to_csv(Lists) when not is_list(Lists) ->
	throw(badarg);
list_to_csv(Lists) when length(Lists) =:= 0 ->
	throw(badarg);
list_to_csv(Lists) ->
	Format = "~p" ++ lists:flatten(lists:duplicate(length(hd(Lists))-1,",~p") ),
	Strs = [lists:flatten(io_lib:format(Format,L) ) ++ "\r\n" || L <- Lists],
	lists:flatten(Strs).
