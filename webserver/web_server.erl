-module(web_server).
-export([start/0]).

start() ->
	inets:start(),
	{ok, Pid} = inets:start(httpd, prop_list() ),
	Pid.

prop_list() ->
	[{port, 80},
	 {server_name, "web_server"},
	 {server_root, "."},
	 {document_root, "."},
	 {bind_address, "localhost"},
	 {directory_index, ["index.html"]}].
