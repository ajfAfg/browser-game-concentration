-module(mod_room).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").

do(ModData) when ModData#mod.request_uri =:= "/room" andalso ModData#mod.method =:= "POST" ->
	io:format("~p~n", [uri_string:dissect_query(ModData#mod.entity_body)]),

	%% wait matching here

	Deck = misc:shuffle(deck:generate_deck() ),
	CSV  = deck:convert_csv(Deck),
	Head = [{code,200}, {content_type,"text/csv"}, {content_length,integer_to_list(length(CSV))}],
	Body = [CSV],
	NewData = [{response, {response,Head,Body}}],
	{proceed, NewData}.
