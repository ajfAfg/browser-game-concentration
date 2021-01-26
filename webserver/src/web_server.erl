-module(web_server).
-export([start/0]).

-include_lib("src/server_config.hrl").

start() ->
	inets:start(),
	inets:start(httpd, prop_list() ).

prop_list() ->
	[{port, 80},
	 {server_name, "web_server"},
	 {server_root, "."},
	 {document_root, "."},
	 {bind_address, ?HOST},
	 {directory_index, ["index.html"]},
	 {mime_types, [
				   {"html", "text/html"},
				   {"css", "text/css"},
				   {"js", "text/javascript"},
				   {"svg", "image/svg+xml"}]}].
