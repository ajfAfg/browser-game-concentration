HOST := localhost

%.beam:%.erl
	erlc -W $<

MODS := ${shell find . -name '*.erl' | sed 's/.erl//'}


.PHONEY: all start stop compile ps clean

# The commands all and start need to execute under root user
all: compile start

start:
	cd ../game && sudo bash -c "erl -noshell -run web_server start $(HOST) -pa ../webserver/ &"

stop:
	ps aux | grep erlang | grep web_server | grep -v grep | awk '{ print "kill -9", $$2 }' | sudo sh

compile: ${MODS:%=%.beam}

ps:
	ps aux | grep erlang | grep -v grep || echo 'no process'

clean:
	rm -rf *.beam *.dump *~ ../game/*.dump
