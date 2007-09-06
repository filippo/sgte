
include ../../conf/include.mk

TEST_PATH = -pa ../../ebin ../ebin

EBIN_FILES = ../ebin/sgte_test_compile.beam \
		../ebin/sgte_test_render.beam \
		../ebin/sgte_test_map.beam \
		../ebin/sgte_test_quiet.beam \
		../ebin/gettext_test.beam \
		../ebin/sgeunit.beam \
		../ebin/eunit_compile.beam \
		../ebin/eunit_render.beam \
		../ebin/eunit_render_map.beam \
		../ebin/eunit_quiet.beam \
		../ebin/eunit_gettext.beam \
		../ebin/run_tests.beam


all: $(EBIN_FILES) run_tests

run_tests:
	$(ERL) -noshell $(TEST_PATH) -run run_tests run_tests -s init stop

clean:
	rm -f ../ebin/*.beam
