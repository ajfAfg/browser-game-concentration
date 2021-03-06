# AP server

## Usage

### Room

You can access `localhost:8080/room` by a HTTP POST request,
and specify a user ID for `user_id` to match other players.

You will get a matching ID if successful (A matching ID is an ID for identifying a match).

~~~bash
$ curl -X POST -d 'user_id=foo' localhost:8080/room
uOfnxkVTu/aoUMUdnBDMg9dPjzXjJ2e5hLLlsrjXANs=
~~~

### Getting a deck

You can access `localhost:8080/deck` by a HTTP POST request,
and specify a user ID and a matching ID for `user_id` and `matching_id` respectively.

You will get a deck represented CSV if successful.

~~~bash
$ curl -X POST -d 'user_id=foo' -d 'matching_id=bar' localhost:8080/deck
diamond,5
spade,8
spade,11
...
spade,5
~~~

### Am I a first player ?

You can access `localhost:8080/first_player` by a HTTP POST request,
and specify a user ID and a matching ID for `user_id` and `matching_id` respectively.

You will know whether are a first if successful.

~~~bash
$ curl -X POST -d 'user_id=foo' -d 'matching_id=bar' localhost:8080/first_player
true
~~~

### Sharing a selected card

You can access `localhost:8080/match` by a HTTP POST request,
and specify a user ID and a matching ID and a current turn for `user_id` and `matching_id` and `turn` respectively.

You will share a selected card among other players if successful.

~~~bash
$ curl -X POST -d 'user_id=foo' -d 'matching_id=bar' -d 'turn=1' localhost:8080/match
1,2
~~~

If you want to tell other players a selected card, access as the code below.

~~~bash
$ curl -X POST -d 'user_id=foo' -d 'matching_id=bar' -d 'turn=1' -d 'x=1' -d 'y=2' localhost:8080/match
ok
~~~

## References

### httpd modules

- [httpd](http://erlang.org/doc/man/httpd.html)
- [inets](http://erlang.org/doc/man/inets.html)
- [HTTP server](http://erlang.org/doc/apps/inets/http_server.html)

### Type specifications

- [Types and Function Specifications](https://erlang.org/doc/reference_manual/typespec.html)

### EUnit

- [EUnit](http://erlang.org/doc/apps/eunit/chapter.html)
- [Erros and Handling](https://erlang.org/doc/reference_manual/errors.html)

### Running server automatically

- [erl](http://erlang.org/doc/man/erl.html)
- [run_erl](https://erlang.org/doc/man/run_erl.html)
