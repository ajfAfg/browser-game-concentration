-module(setup).
-export([setup/0]).

-include("server_config.hrl").

setup() ->
    Fun = fun() ->
				  web_server:start(?HOST)
		  end,
	spawn(Fun).
