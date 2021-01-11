EUNIT := ${shell find /usr/local/Cellar/erlang/ -type d -name ebin | grep eunit}
INCLUDE := ../
EBIN := ${shell mkdir -p ebin ; echo ebin}
ERLCFLAGS := -W -pa $(EUNIT) -I $(INCLUDE) -o $(EBIN)
ERLC := erlc $(ERLCFLAGS)

EBINPATH := ../ebin $(EBIN)
ERLFLAGS := -pa $(EBINPATH) -noshell
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
