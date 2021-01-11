include ../Makefile

DOMAIN := localhost

%.beam:%.erl
	erlc -W $<

MODS := ${shell find . -name '*.erl' | sed 's/.erl//'}


.PHONEY: all start stop compile clean

# The commands all and start need to execute under root user
all: compile start

start:
	cd ../game && sudo bash -c "erl -noshell -run web_server start $(DOMAIN) -pa ../webserver/ &"

stop:
	ps aux | grep erlang | grep web_server | grep -v grep | awk '{ print "kill -9", $$2 }' | sudo sh

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam *.dump *~ ../game/*.dump
