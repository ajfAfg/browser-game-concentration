include ../Makefile

DOMAIN := localhost

EBIN := ${shell mkdir -p ebin ; echo ebin}
ERLCFLAGS := -W -o $(EBIN)
ERLC := erlc $(ERLCFLAGS)

#EBINPATH := $(EBIN)
#ERLFLAGS := -noshell
#ERLFLAGS += -run application load concentration
#ERLFLAGS += -run application start concentration
#ERLFLAGS += -run ap_server start $(DOMAIN)
#ERLFLAGS += -pa $(EBINPATH)
#ERL := erl $(ERLFLAGS)

PIPE_DIR := /tmp/
LOG_DIR := ${shell mkdir -p log ; echo log}
EBIN_PATH := $(EBIN)
INCLUDE := ${shell mkdir -p include; echo include}
ERLD_FLAGS := $(PIPE_DIR) $(LOG_DIR)
ERLD_FLAGS += "erl -run setup setup -pa $(EBIN) $(INCLUDE)"
ERLD := run_erl -daemon $(ERLD_FLAGS)

MODS := ${shell find src -name '*.erl' | sed 's/.erl//'}

%.beam: %.erl
	$(ERLC) $<

.PHONY: all start stop compile clean

all: compile start

start:
	$(ERLD)

stop:
	ps aux | grep erlang | grep setup | grep -v grep | awk '{ print "kill -9", $$2 }' | sh

compile: ${MODS:%=%.beam}

clean:
	rm -rf ebin/*.beam *.dump log/*
