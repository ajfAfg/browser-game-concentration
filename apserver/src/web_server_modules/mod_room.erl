-module(mod_room).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").
-include_lib("src/server_config.hrl").

do(ModData) ->
	case ModData#mod.request_uri =:= "/room" andalso ModData#mod.method =:= "POST" of
		true ->
			handle(ModData);
		false ->
			{proceed, ModData}
	end.

handle(ModData) ->
	EntityList = uri_string:dissect_query(ModData#mod.entity_body),
	UserId = misc:get_entity_with_tag(EntityList, "user_id"),
	MatchingId = case room_server:wait_for_matching(UserId) of
					 {matching, Id} ->
						 Id;
					 no_matching ->
						 "no_matching"
				 end,
	Head = [{code,200}, {content_type,"text/plain"}, {content_length,misc:len(MatchingId)}, ?ACAO],
	Body = [MatchingId],
	NewData = [{response, {response,Head,Body}}],
	{break, NewData}.
