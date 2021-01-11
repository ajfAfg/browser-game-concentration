EBIN := ebin
ERLCFLAGS := -W -o $(EBIN)
ERLC := erlc $(ERLCFLAGS)

SRCPATH := ebin
ERLFLAGS := -noshell -run ap_server start -pa $(SRCPATH)
ERL := erl $(ERLFLAGS)

MODS := ${shell find src -name '*.erl' | sed 's/.erl//'}

include ../Makefile

%.beam:%.erl
	$(ERLC) $<

.PHONY: all start stop compile clean

all: compile start

start:
	$(ERL) &

stop:
	ps aux | grep erlang | grep ap_server | grep -v grep | awk '{ print "kill -9", $$2 }' | sh

compile: ${MODS:%=%.beam}

clean:
	rm -rf ebin/*.beam *.dump
