
include conf/include.mk

SHELL = /bin/sh

#tags command and options
TAG_CMD = etags
TAG_FLAGS = 

TEST_PATH = -pa ../../ebin ../ebin

LIB_FILES = /usr/local/lib/erlang/lib/sg*/*.erl /usr/local/lib/erlang/lib/sg*/src/*.erl

SRC_FILES = src/sgte.erl src/sgte_render.erl src/sgte_parse.erl

#test
TEST_OBJS = test/ebin/sgte_test_compile.beam \
		test/ebin/sgte_test_render.beam \
		test/ebin/sgte_test_map.beam \
		test/ebin/run_tests.beam \
		test/ebin/sgeunit.beam

TAG_FILES = $(LIB_FILES) $(SRC_FILES)

EBIN_DIR = ebin/

#Edoc variables
DOCDIR = doc

.PHONY: all conf conf_clean test docs unittest

all: conf compile

compile: 
	cd src&& $(MAKE)

conf:
	cd conf&& $(MAKE)

conf_clean:
	cd conf&& $(MAKE) clean


test: unittest
	cd test/src; \
	$(ERL) -noshell $(TEST_PATH) -run run_tests run_tests -s init stop

docs: $(SRC_FILES)
	erl -noshell -run edoc_run files ["'$<'"] \
		'[{dir,"$(DOCDIR)"}]' -s init stop
tags: src/*.erl
	cd src/ && $(TAG_CMD) $(TAG_FLAGS) $(TAG_FILES)

unittest: conf compile
	cd test/src&& $(MAKE)

clean:
	rm src/*.beam; rm ebin/*.beam; rm test/src/*.beam; rm test/ebin/*.beam

