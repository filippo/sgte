SHELL = /bin/sh

#tags command and options
TAG_CMD = etags
TAG_FLAGS = 

#erlang compiler
ERLC_CMD = /usr/bin/erlc
ERL_CMD = /usr/bin/erl

LIB_FILES = /usr/lib/erlang/lib/sg*/*.erl /usr/lib/erlang/lib/sg*/src/*.erl

SRC_FILES = src/sgte.erl
OBJ_FILES = ebin/sgte.beam

#test
TEST_OBJS = test/ebin/sgte_test.beam test/ebin/run_tests.beam test/ebin/sgeunit.beam

TAG_FILES = $(LIB_FILES) $(SRC_FILES)

EBIN_DIR = ebin/

#Edoc variables
DOCDIR = doc

all: compile

test: unittest
	$(ERL_CMD) -noshell -run run_tests -s init stop|grep \[ERROR\]

docs: $(SRC_FILES)
	erl -noshell -run edoc_run files ["'$<'"] \
		'[{dir,"$(DOCDIR)"}]' -s init stop
tags: src/*.erl
	cd src/ && $(TAG_CMD) $(TAG_FLAGS) $(TAG_FILES)

unittest: $(TEST_OBJS)

test/ebin/%.beam: test/src/%.erl
	$(ERLC_CMD) -o test/$(EBIN_DIR) $<

compile: $(OBJ_FILES)

ebin/%.beam: src/%.erl
	$(ERLC_CMD) -o $(EBIN_DIR) $<

clean:
	rm src/*.beam; rm ebin/*.beam; rm test/src/*.beam; rm test/ebin/*.beam

