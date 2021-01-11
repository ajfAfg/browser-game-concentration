EUNIT := /usr/local/Cellar/erlang/23.1.3/lib/erlang/lib/eunit-2.6/ebin
INCLUDE := ../
EBIN := ebin
ERLCFLAGS := -W -pa $(EUNIT) -I $(INCLUDE) -o $(EBIN)
ERLC := erlc $(ERLCFLAGS)

SRCPATH := ../ebin ebin
ERLFLAGS := -pa $(SRCPATH) -noshell
ifeq ($(VERBOSE), true)
	ERLFLAGS += -eval "eunit:test({}, [verbose])" -s init stop
else
	ERLFLAGS += -eval "eunit:test({}, [])" -s init stop
endif
ERL := erl $(ERLFLAGS)

MODS := ${shell find src -name '*.erl' | sed 's/.erl//'}

%.beam: %.erl
	$(ERLC) $<

.PHONY: all check compile clean

all: compile check

check:
	find src -name '*.erl' -exec basename {} '.erl' \; | xargs -I{} -n1 $(ERL)

compile: ${MODS:%=%.beam}
	cd ../ && make compile

clean:
	rm -rf ebin/*.beam *.dump
