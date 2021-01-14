-module(mod_match).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").

do(ModData) ->
	case ModData#mod.request_uri =:= "/match" andalso ModData#mod.method =:= "POST" of
		true ->
			handle(ModData);
		false ->
			{proceed, ModData}
	end.

handle(ModData) ->
	EntityList = uri_string:dissect_query(ModData#mod.entity_body),
	UserId = misc:get_entity_with_tag(EntityList, "user_id"),
	MatchingId = misc:get_entity_with_tag(EntityList, "matching_id"),
	Reply = case 
				{
				 misc:get_entity_with_tag(EntityList, "x"),
				 misc:get_entity_with_tag(EntityList, "y")
				}
			of
				{"", ""} ->
					fetch_opponent_move(MatchingId);
				Move ->
					ok = sharing_move_server:share_this_move(MatchingId, Move),
					"ok"
			end,
	Head = [{code,200}, {content_type,"text/plain"}, {content_length,misc:len(Reply)}],
	Body = [Reply],
	NewData = [{response, {response,Head,Body}}],
	{break, NewData}.

fetch_opponent_move(MatchingId) ->
	case sharing_move_server:fetch_opponent_move(MatchingId) of
		timeout ->
			"timeout";
		{X, Y} ->
			%% X and Y are the type string
			List = [[list_to_integer(X), list_to_integer(Y)]],
			csv:list_to_csv(List)
	end.
