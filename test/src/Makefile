
include ../../conf/include.mk

TEST_PATH = -pa ../../ebin ../ebin

EBIN_FILES = ../ebin/eunit_compile.beam \
		../ebin/eunit_render.beam \
		../ebin/eunit_render_str.beam \
		../ebin/eunit_render_bin.beam \
		../ebin/eunit_render_map.beam \
		../ebin/eunit_render_types.beam \
		../ebin/eunit_quiet.beam \
		../ebin/eunit_gettext.beam \
		../ebin/eunit_nested.beam \
		../ebin/run_tests.beam


all: $(EBIN_FILES) run_tests

run_tests:
	$(ERL) -noshell $(TEST_PATH) -run run_tests run_tests -s init stop

clean:
	rm -f ../ebin/*.beam
