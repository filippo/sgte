-module(run_tests).

-export([run_tests/0, run_tests/1]).

run_tests() ->
    run_tests([eunit_compile, 
               eunit_render, 
               eunit_render_str,
               eunit_render_bin,
               eunit_render_map,
               eunit_render_types,
               eunit_quiet,
               eunit_gettext,
               eunit_nested]).

run_tests([]) ->
    done;
run_tests([M|T]) ->
    io:format("Running ~p~n", [M]),
    M:test(),
    run_tests(T).
