-module(mod_test2).
-export([do/1]).
%-include_lib("inets/src/httpd.hrl").
%-include_lib("inets/src/http_server/httpd.hrl").
-include_lib("inets/include/httpd.hrl").

do(ModData) ->
	io:format("~p~n", [ModData]),
	%io:format("~p~n", [ModData#mod.request_uri]),

	Head = [{code,200}, {content_type,"text/plain"}, {content_length,"3"}],
	case ModData#mod.request_uri of
		"/foo" ->
			Body = ["ok!"];
		_ ->
			Body = ["Umm"]
	end,
	NewData = [{response, {response,Head,Body}}],
	timer:sleep(10000),
	io:format("okok~n"),
	{proceed, NewData}.
%	{break, NewData}.
