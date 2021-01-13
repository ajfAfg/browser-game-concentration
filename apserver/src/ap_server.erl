-module(ap_server).
-export([start/1]).

%start([Host | _]) ->
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
%	 {modules, [mod_test2]}].
%	 {modules, [mod_alias, mod_auth, mod_esi, mod_actions, mod_cgi, mod_dir, mod_get, mod_head, mod_log, mod_disk_log, mod_test2]}].
	 {modules, [mod_alias, mod_auth, mod_esi, mod_actions, mod_head, mod_log, mod_disk_log, mod_room, mod_dealer]}].
