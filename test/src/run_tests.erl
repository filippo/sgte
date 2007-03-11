-module(run_tests).

-export([start/0, run_tests/1]).

start() ->
    run_tests([sgte_test]).
run_tests([]) ->
    done;
run_tests([H|T]) ->
    sgeunit:run(H),
    run_tests(T).
