-module(web_server).
-export([start/1]).

start([Host | _]) ->
	inets:start(),
	{ok, Pid} = inets:start(httpd, prop_list(Host) ),
	Pid.

prop_list(Host) ->
	[{port, 80},
	 {server_name, "web_server"},
	 {server_root, "."},
	 {document_root, "."},
	 {bind_address, Host},
	 {directory_index, ["index.html"]},
	 {mime_types, [
				   {"html", "text/html"},
				   {"css", "text/css"},
				   {"js", "text/javascript"},
				   {"svg", "image/svg+xml"}]}].
