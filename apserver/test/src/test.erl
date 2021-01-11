-module(test).

-include_lib("eunit/include/eunit.hrl").

first_test() ->
	?_assertEqual(ok, io:format(user,"ok!",[]) ).

second_test() ->
	?_assertEqual(1+1, 2).

third_test() ->
%	?_assert(1+1 =:= 100000000).
	?_assertEqual(ok, ok).
