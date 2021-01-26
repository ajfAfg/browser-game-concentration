-module(mod_first_player).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").
-include_lib("src/server_config.hrl").

do(ModData) ->
	case ModData#mod.request_uri =:= "/first_player" andalso ModData#mod.method =:= "POST" of
		true ->
			handle(ModData);
		false ->
			{proceed, ModData}
	end.

handle(ModData) ->
	EntityList = uri_string:dissect_query(ModData#mod.entity_body),
	MathingId = misc:get_entity_with_tag(EntityList, "matching_id"),
	UserId = misc:get_entity_with_tag(EntityList, "user_id"),

	Reply = atom_to_list(deciding_first_player_server:is_first_player(MathingId, UserId) ),
	Head = [{code,200}, {content_type,"text/plain"}, {content_length,misc:len(Reply)}, ?ACAO],
	Body = [Reply],
	NewData = [{response, {response,Head,Body}}],
	{break, NewData}.
