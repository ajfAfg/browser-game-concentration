-module(ap_server).
-export([start/0]).

start() ->
	inets:start(),
	{ok, Pid} = inets:start(httpd, prop_list() ),
	Pid.

prop_list() ->
	[{port, 8080},
	 {server_name, "ap_server"},
	 {server_root, "."},
	 {document_root, "."},
	 {bind_address, "localhost"},
%	 {modules, [mod_test2]}].
%	 {modules, [mod_alias, mod_auth, mod_esi, mod_actions, mod_cgi, mod_dir, mod_get, mod_head, mod_log, mod_disk_log, mod_test2]}].
	 {modules, [mod_alias, mod_auth, mod_esi, mod_actions, mod_head, mod_log, mod_disk_log, mod_room]}].
