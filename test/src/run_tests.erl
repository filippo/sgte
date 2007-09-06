-module(run_tests).

-export([run_tests/0, run_tests/1]).

run_tests() ->
    run_tests([eunit_compile, 
                     eunit_render, 
                     eunit_render_map,
                     eunit_quiet,
                     eunit_gettext]).

run_tests([]) ->
    done;
run_tests([M|T]) ->
    io:format("Running ~p~n", [M]),
    M:test(),
    run_tests(T).
