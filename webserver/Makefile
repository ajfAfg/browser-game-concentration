EBIN := ${shell mkdir -p ebin ; echo ebin}
ERLCFLAGS := -W -o $(EBIN)
ERLC := erlc $(ERLCFLAGS)

PIPE_DIR := /tmp/
LOG_DIR := ${shell mkdir -p log ; echo log}
EBIN_PATH := ../webserver/$(EBIN)
ERLD_FLAGS := $(PIPE_DIR) $(LOG_DIR)
ERLD_FLAGS += "erl -run web_server start -pa $(EBIN_PATH)"
ERLD := run_erl -daemon $(ERLD_FLAGS)

MODS := ${shell find src -name '*.erl' | sed 's/.erl//'}

%.beam: %.erl
	$(ERLC) $<

.PHONY: all start start2 stop compile ps clean

# The commands all and start need to execute under root user
all: compile start

start:
	cd ../game && sudo $(ERLD)

# use this target instead of the target start if a processing of the target fails
start2:
	cd ../game && sudo bash -c "erl -noshell -run web_server start -pa ../webserver/ebin &"

stop:
	ps aux | grep erlang | grep web_server | grep -v grep | awk '{ print "kill -9", $$2 }' | sudo sh

compile: ${MODS:%=%.beam}

ps:
	ps aux | grep erlang | grep -v grep || echo 'no process'

clean:
	rm -rf ebin/*.beam ../game/*.dump log/*
