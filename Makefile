
#include conf/include.mk

LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.7.1

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

compile: conf
	mkdir -p ebin/
	cd src && $(MAKE)

conf:
	cd conf && $(MAKE)

conf_clean:
	cd conf && $(MAKE) clean


test: conf compile
	cd test/src&& $(MAKE)

docs: $(DOCDIR)/index.html

$(DOCDIR)/index.html: $(SRC_FILES)
	erl -noshell -run edoc_run application "sgte" '"."' '[]'

tags: src/*.erl
	cd src/ && $(TAG_CMD) $(TAG_FLAGS) $(TAG_FILES)

clean:
	rm -rfv rm ebin/*.beam test/src/*.beam test/ebin/*.beam conf/autom4te.cache conf/config.log conf/config.status conf/configure conf/include.mk doc/*.html doc/*.css doc/*.png doc/edoc-info

package: clean
	@mkdir sgte-$(VERSION)/ && cp -rf _build.cfg CHANGELOG ebin conf doc Makefile sgte.pub src test sgte-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf sgte-$(VERSION).tgz sgte-$(VERSION)
	@rm -rf sgte-$(VERSION)/

install:
	mkdir -p $(prefix)/$(LIBDIR)/sgte-$(VERSION)/ebin
	for i in ebin/*.beam; do install $$i $(prefix)/$(LIBDIR)/sgte-$(VERSION)/$$i ; done

