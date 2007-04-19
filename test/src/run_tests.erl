-module(run_tests).

-export([init/0, run_tests/1]).

init() ->
    run_tests([sgte_render, sgte_compile]).
run_tests([]) ->
    done;
run_tests([H|T]) ->
    sgeunit:run(H, {verbose}),
    run_tests(T).
