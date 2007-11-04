-module(eunit_gettext).

-include_lib("eunit/include/eunit.hrl").

%%--------------------
%%
%% Tests
%%
%%--------------------

%% Setup Tests
setup() ->
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
compile_test_() ->
    {ok, C} = sgte:compile(simple()),
    ?_assert(C =:= [{gettext, "Hello World", 1}]).

%% Test Render
do_test_() ->
    {setup, fun setup/0, [fun simple_it/0,
                          fun simple_se/0,
                          fun simple_en/0,
                          fun simple_undef/0,
                          fun no_lc/0]}.

simple_it() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render_str(C, [], [{gettext_lc, "it"}]),
    ?_assert("Ciao Mondo" =:= Res).

simple_se() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render_str(C, [], [{gettext_lc, "se"}]),
    ?_assert("Hej V\344rld" =:= Res).

simple_en() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render_str(C, [], [{gettext_lc, "en"}]),
    ?_assert("Hello World" =:= Res).

simple_undef() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render_str(C, [{gettext_lc, "aa"}], [quiet]),
    ?_assert("Hello World" =:= Res).

%% No language code passed
no_lc() ->
    {ok, C} = sgte:compile(simple()),
    Res = sgte:render_str(C, [], [quiet]),
    ?_assert("Hello World" =:= Res).

%%--------------------
%%
%% Internal functions
%%
%%--------------------
%% Simple Template String
simple() ->
    "$txt:{Hello World}$".
