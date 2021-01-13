-module(setup).
-export([setup/0]).

-define(HOST, "localhost").

setup() ->
    Fun = fun() ->
				  application:load(concentration),
				  application:start(concentration),
				  ap_server:start(?HOST)
		  end,
	spawn(Fun).
