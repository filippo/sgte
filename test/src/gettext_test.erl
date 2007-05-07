-module(gettext_test).

-export([setup_test/0]).
-export([test_compile/0]).
-export([test_simple_it/0, 
	 test_simple_se/0, 
	 test_simple_en/0, 
	 test_simple_undef/0, 
	 test_no_lc/0]).

%%--------------------
%%
%% Tests
%%
%%--------------------

%% Setup Tests
setup_test() ->
    try
	gettext_server:start(),
	{_, SeBin} = file:read_file("../priv/gettext_test/swedish.po"),
	{_, ItBin} = file:read_file("../priv/gettext_test/italian.po"),
	ok = gettext:store_pofile("se", SeBin),
	ok = gettext:store_pofile("it", ItBin)
    catch
	_Err:_Reason ->
	    error
    end.

%% Test Compile
test_compile() ->
    {ok, C} = sgte:compile(simple()),
    sgeunit:assert_equal([{gettext, "Hello World", 1}], C).

%% Test Render
test_simple_it() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render(C, [{gettext_lc, "it"}]),
    sgeunit:assert_equal("Ciao Mondo", Res).

test_simple_se() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render(C, [{gettext_lc, "se"}]),
    sgeunit:assert_equal("Hej V\344rld", Res).

test_simple_en() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render(C, [{gettext_lc, "en"}]),
    sgeunit:assert_equal("Hello World", Res).

test_simple_undef() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render(C, [{gettext_lc, "aa"}]),
    sgeunit:assert_equal("Hello World", Res).

%% No language code passed
test_no_lc() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render(C, []),
    sgeunit:assert_equal("Hello World", Res).

%%--------------------
%%
%% Internal functions
%%
%%--------------------
%% Simple Template String
simple() ->
    "$txt:{Hello World}$".
