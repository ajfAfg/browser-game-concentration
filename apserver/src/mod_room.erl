-module(mod_room).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").

do(ModData) when ModData#mod.request_uri =:= "/room" andalso ModData#mod.method =:= "POST" ->
	io:format("~p~n", [uri_string:dissect_query(ModData#mod.entity_body)]),
	Deck = "spade,10\r\n" ++ "heart,2\r\n",
	Head = [{code,200}, {content_type,"text/csv"}, {content_length,integer_to_list(length(Deck))}],
	Body = [Deck],
	NewData = [{response, {response,Head,Body}}],
	{proceed, NewData}.
