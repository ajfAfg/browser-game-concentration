-module(mod_dealer).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").

do(ModData) ->
	case ModData#mod.request_uri =:= "/dealer" andalso ModData#mod.method =:= "POST" of
		true ->
			handle(ModData);
		false ->
			{proceed, ModData}
	end.

handle(ModData) ->
	EntityList = uri_string:dissect_query(ModData#mod.entity_body),
%	UserId = misc:get_entity_with_tag(EntityList, "user_id"),
	MathingId = misc:get_entity_with_tag(EntityList, "matching_id"),
			   
	CSV = case providing_deck_server:request_deck(MathingId) of
			  no_deck ->
				  "no_deck";
			  Deck ->
				  deck:convert_csv(Deck)
		  end,
	Head = [{code,200}, {content_type,"text/csv"}, {content_length,misc:len(CSV)}],
	Body = [CSV],
	NewData = [{response, {response,Head,Body}}],
	{break, NewData}.
