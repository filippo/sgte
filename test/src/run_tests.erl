-module(run_tests).

-export([run_tests/0, run_tests/1]).

run_tests() ->
    run_tests([sgte_render, sgte_compile]).
run_tests([]) ->
    done;
run_tests([H|T]) ->
    sgeunit:run(H, {verbose}),
    run_tests(T).
