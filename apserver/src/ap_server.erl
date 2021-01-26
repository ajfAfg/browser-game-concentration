-module(ap_server).
-export([start/0]).

-include_lib("src/server_config.hrl").

start() ->
	inets:start(),
	inets:start(httpd, prop_list() ),
	application:load(concentration),
	application:start(concentration).

prop_list() ->
	[{port, 8080},
	 {server_name, "ap_server"},
	 {server_root, "."},
	 {document_root, "."},
	 {bind_address, ?HOST},
	 {modules, [mod_alias, mod_auth, mod_esi, mod_actions, mod_head, mod_log, mod_disk_log, mod_room, mod_deck, mod_match, mod_first_player]}].
