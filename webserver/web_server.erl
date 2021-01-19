-module(web_server).
-export([start/1]).

start([Domain | _]) ->
	inets:start(),
	{ok, Pid} = inets:start(httpd, prop_list(Domain) ),
	Pid.

prop_list(Domain) ->
	[{port, 80},
	 {server_name, "web_server"},
	 {server_root, "."},
	 {document_root, "."},
	 {bind_address, Domain},
	 {directory_index, ["index.html"]},
	 {mime_types, [
				   {"html", "text/html"},
				   {"css", "text/css"},
				   {"js", "text/javascript"},
				   {"svg", "image/svg+xml"}]}].
