-module(run_tests).

-export([run_tests/0, run_tests/1]).

run_tests() ->
    run_tests([sgte_test_compile, 
	       sgte_test_render, 
	       sgte_test_map, 
	       sgte_test_quiet,
	       gettext_test]).
run_tests([]) ->
    done;
run_tests([H|T]) ->
    sgeunit:run(H, {verbose}),
    run_tests(T).
