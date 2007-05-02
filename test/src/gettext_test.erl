-module(gettext_test).

-export([setup_test/0]).
-export([test_compile/0]).
-export([test_simple_it/0, test_simple_se/0, test_simple_en/0, test_simple_undef/0]).

%%--------------------
%%
%% Tests
%%
%%--------------------

%% Setup Tests
setup_test() ->
    gettext_server:start(),
    {_, SeBin} = file:read_file("../priv/gettext_test/swedish.po"),
    {_, ItBin} = file:read_file("../priv/gettext_test/italian.po"),
    ok = gettext_server:store_pofile("se", SeBin),
    ok = gettext_server:store_pofile("it", ItBin).

%% Test Compile
test_compile() ->
    {ok, C} = sgte:compile(simple()),
    sgeunit:assert_equal(C, [{gettext, "Hello World", 1}]).

%% Test Render
test_simple_it() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render(C, [{gettext_lc, "it"}]),
    sgeunit:assert_equal(Res, "Ciao Mondo").

test_simple_se() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render(C, [{gettext_lc, "se"}]),
    sgeunit:assert_equal(Res, "Hej V\344rld").

test_simple_en() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render(C, [{gettext_lc, "en"}]),
    sgeunit:assert_equal(Res, "Hello World").

test_simple_undef() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render(C, [{gettext_lc, "aa"}]),
    sgeunit:assert_equal(Res, "Hello World").

%%--------------------
%%
%% Internal functions
%%
%%--------------------
%% Simple Template String
simple() ->
    "$txt:{Hello World}$".
