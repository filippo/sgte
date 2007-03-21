-module(run_tests).

-export([start_link/0, run_tests/1]).

start_link() ->
    sin_task_manager:start_task(test),
    run_tests([sgte_test]),
    sin_task_manager:finish_task(test).
run_tests([]) ->
    done;
run_tests([H|T]) ->
    sgeunit:run(H, {verbose}),
    run_tests(T).
