-module(eunit_gettext).

-export([gettext_dir/0]).

-include_lib("eunit/include/eunit.hrl").

%% The gettext_server module needs to know where it should store the language data.
%% There are two alternatives:
%%      - setting the environment variable GETTEXT_DIR, or...
%%      - implement this callback and call gettext_server/1 or gettext_server/2 giving ?MODULE in the first parameter
gettext_dir() ->
        "../priv/gettext_server_dir/".

%%--------------------
%%
%% Tests
%%
%%--------------------

%% Setup Tests
setup() ->
    try
        {ok, _} = gettext_server:start(?MODULE),
        {_, SeBin} = file:read_file("../priv/gettext_test/swedish.po"),
        {_, ItBin} = file:read_file("../priv/gettext_test/italian.po"),
        ok = gettext:store_pofile("se", SeBin),
        ok = gettext:store_pofile("it", ItBin)
    catch
        _Err:_Reason = Error ->
                % We need to ensure that the gettext_server is taken down to prevent misleading results
                exit(whereis(gettext_server), kill),
                ErrorStr = lists:flatten(["The test suite setup could not be completed due to: ", io_lib:write(Error)]),
                erlang:display(ErrorStr),
            error
    end.

%% Test Compile
compile_test_() ->
    {ok, C} = sgte:compile(simple()),
    ?_assert(C =:= [{gettext, "Hello World", 1}]).

%% Test Render
do_test_() ->
    {setup,
                fun setup/0,
                [
                        fun simple_it/0,
            fun simple_se/0,
            fun simple_en/0,
            fun simple_undef/0,
            fun no_lc/0
                ]}.

simple_it() ->
    {ok, C} = sgte:compile(simple()),
        Res = sgte:render_str(C, [], [{gettext_lc, "it"}]),
        ?assert("Ciao Mondo" =:= Res).

simple_se() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render_str(C, [], [{gettext_lc, "se"}]),
    ?assert("Hej V\344rld" =:= Res).

simple_en() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render_str(C, [], [{gettext_lc, "en"}]),
    ?assert("Hello World" =:= Res).

simple_undef() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render_str(C, [{gettext_lc, "aa"}], [quiet]),
    ?assert("Hello World" =:= Res).

%% No language code passed
no_lc() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render_str(C, [], [quiet]),
    ?assert("Hello World" =:= Res).

%%--------------------
%%
%% Internal functions
%%
%%--------------------
%% Simple Template String
simple() ->
    "$txt:{Hello World}$".
