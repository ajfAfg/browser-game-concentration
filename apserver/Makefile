include ../Makefile

DOMAIN := localhost

EBIN := ${shell mkdir -p ebin ; echo ebin}
ERLCFLAGS := -W -o $(EBIN)
ERLC := erlc $(ERLCFLAGS)

EBINPATH := $(EBIN)
#ERLFLAGS := -noshell -run ap_server start $(DOMAIN) -pa $(EBINPATH)
ERLFLAGS := -noshell
ERLFLAGS += -run application load concentration
ERLFLAGS += -run application start concentration
ERLFLAGS += -run ap_server start $(DOMAIN)
ERLFLAGS += -pa $(EBINPATH)
ERL := erl $(ERLFLAGS)

MODS := ${shell find src -name '*.erl' | sed 's/.erl//'}

%.beam: %.erl
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
