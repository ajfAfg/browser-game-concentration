-module(setup).
-export([setup/0]).

-include_lib("src/server_config.hrl").

setup() ->
    Fun = fun() ->
				  application:load(concentration),
				  application:start(concentration),
				  ap_server:start(?HOST)
		  end,
	spawn(Fun).
