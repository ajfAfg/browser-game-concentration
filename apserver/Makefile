EBIN := ${shell mkdir -p ebin ; echo ebin}
ERLCFLAGS := -W -o $(EBIN)
ERLC := erlc $(ERLCFLAGS)

PIPE_DIR := /tmp/
LOG_DIR := ${shell mkdir -p log ; echo log}
EBIN_PATH := $(EBIN)
ERLD_FLAGS := $(PIPE_DIR) $(LOG_DIR)
ERLD_FLAGS += "erl -run ap_server start -pa $(EBIN_PATH)"
ERLD := run_erl -daemon $(ERLD_FLAGS)

MODS := ${shell find src -name '*.erl' | sed 's/.erl//'}

%.beam: %.erl
	$(ERLC) $<

.PHONY: all start stop compile ps clean

all: compile start

start:
	$(ERLD)

stop:
	ps aux | grep erlang | grep ap_server | grep -v grep | awk '{ print "kill -9", $$2 }' | sh

compile: ${MODS:%=%.beam}

ps:
	ps aux | grep erlang | grep -v grep || echo 'no process'

clean:
	rm -rf ebin/*.beam *.dump log/*
