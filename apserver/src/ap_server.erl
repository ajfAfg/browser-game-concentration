-module(ap_server).
-export([start/1]).

start(Host) ->
	inets:start(),
	{ok, Pid} = inets:start(httpd, prop_list(Host) ),
	Pid.

prop_list(Host) ->
	[{port, 8080},
	 {server_name, "ap_server"},
	 {server_root, "."},
	 {document_root, "."},
	 {bind_address, Host},
	 {modules, [mod_alias, mod_auth, mod_esi, mod_actions, mod_head, mod_log, mod_disk_log, mod_room, mod_dealer, mod_match, mod_first_player]}].
