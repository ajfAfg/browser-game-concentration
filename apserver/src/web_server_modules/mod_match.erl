-module(mod_match).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").
-include_lib("src/server_config.hrl").

do(ModData) ->
	case ModData#mod.request_uri =:= "/match" andalso ModData#mod.method =:= "POST" of
		true ->
			handle(ModData);
		false ->
			{proceed, ModData}
	end.

handle(ModData) ->
	EntityList = uri_string:dissect_query(ModData#mod.entity_body),
	MatchingId = misc:get_entity_with_tag(EntityList, "matching_id"),
	UserId = misc:get_entity_with_tag(EntityList, "user_id"),
	Turn = list_to_integer(misc:get_entity_with_tag(EntityList, "turn") ),
	Fun = case
			  {
			   misc:get_entity_with_tag(EntityList, "x"),
			   misc:get_entity_with_tag(EntityList, "y")
			  }
		  of
			  {"", ""} ->
				  fun() ->
						  non_turn_player:know_turn_player_move(MatchingId, UserId, Turn)
				  end;
			  Move ->
				  fun() ->
						  turn_player:tell_other_players_my_move(MatchingId, UserId, Turn, Move)
				  end
		  end,
	Reply = case Fun() of
				Atom when is_atom(Atom) ->
					atom_to_list(Atom);
				{X, Y} ->
					List = [[list_to_integer(X), list_to_integer(Y)]],
					csv:list_to_csv(List)
			end,
	Head = [{code,200}, {content_type,"text/csv"}, {content_length,misc:len(Reply)}, ?ACAO],
	Body = [Reply],
	NewData = [{response, {response,Head,Body}}],
	{break, NewData}.
