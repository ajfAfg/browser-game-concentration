-module(mod_room).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").

do(ModData) when ModData#mod.request_uri =:= "/room" andalso ModData#mod.method =:= "POST" ->
	EntityList = uri_string:dissect_query(ModData#mod.entity_body),
	UserId = misc:get_entity_with_tag(EntityList, "id"),
	MatchingId = room_server:match(UserId),
	Head = [{code,200}, {content_type,"text/plain"}, {content_length,misc:len(MatchingId)}],
	Body = [MatchingId],
	NewData = [{response, {response,Head,Body}}],

%	Deck = misc:shuffle(deck:generate_deck() ),
%	CSV  = deck:convert_csv(Deck),
%	Head = [{code,200}, {content_type,"text/csv"}, {content_length,integer_to_list(length(CSV))}],
%	Body = [CSV],
%	NewData = [{response, {response,Head,Body}}],
	{proceed, NewData}.
