
#include conf/include.mk

SHELL = /bin/sh

#tags command and options
TAG_CMD = etags
TAG_FLAGS = 

LIB_FILES = /usr/local/lib/erlang/lib/sg*/*.erl /usr/local/lib/erlang/lib/sg*/src/*.erl

SRC_FILES = src/sgte.erl src/sgte_render.erl src/sgte_parse.erl

TAG_FILES = $(LIB_FILES) $(SRC_FILES)

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


test: conf compile
	cd test/src&& $(MAKE)

docs: $(SRC_FILES)
	erl -noshell -run edoc_run files \
		["'src/sgte.erl', 'src/sgte_parse.erl', 'src/sgte_render.erl', 'src/sgte_gettext.erl', 'src/sgte_dict.erl'"] \
		'[{dir,"$(DOCDIR)"}]' -s init stop
tags: src/*.erl
	cd src/ && $(TAG_CMD) $(TAG_FLAGS) $(TAG_FILES)

clean:
	rm src/*.beam; rm ebin/*.beam; rm test/src/*.beam; rm test/ebin/*.beam

