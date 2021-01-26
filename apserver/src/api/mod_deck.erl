-module(mod_deck).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").
-include_lib("src/server_config.hrl").

do(ModData) ->
	case ModData#mod.request_uri =:= "/deck" andalso ModData#mod.method =:= "POST" of
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
			  Deck when is_list(Deck) ->
				  deck:convert_csv(Deck);
			  Other ->
				  io:format("~p: ~p~n", [MathingId, Other]),
				  "false"
		  end,
	Head = [{code,200}, {content_type,"text/csv"}, {content_length,misc:len(CSV)}, ?ACAO],
	Body = [CSV],
	NewData = [{response, {response,Head,Body}}],
	{break, NewData}.
