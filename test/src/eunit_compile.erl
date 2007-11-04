-module(eunit_compile).

-include_lib("eunit/include/eunit.hrl").

%%--------------------
%%
%% Tests
%%
%%--------------------
attr_test_() ->
    Str = "foo $bar$ baz",
    {ok, Compiled} = sgte:compile(Str),
    ?_assert(Compiled =:= [<<"foo ">>, {attribute, [bar], 1}, <<" baz">>]).

multi_attr_test_() ->
    Str = "$foo.bar.baz$",
    {ok, Compiled} = sgte:compile(Str),
    ?_assert(Compiled =:= [{attribute, [foo, bar, baz], 1}]).

multiline_attr_test_() ->
    Str = "foo 
$bar
$
 baz",
    {ok, Compiled} = sgte:compile(Str),
    ?_assert(Compiled =:= [<<"foo \n">>, {attribute, [bar], 2}, <<"\n baz">>]).

include_test_() ->
    {ok, C} = sgte:compile("foo $include tmpl$ baz"),
    ?_assert(C == [<<"foo ">>, {include, [tmpl], 1}, <<" baz">>]).

include_multiline_test_() ->
    Str = "foo $include
tmpl$ baz",
    {ok, C} = sgte:compile(Str),
    ?_assert(C =:= [<<"foo ">>, {include, [tmpl], 1}, <<" baz">>]).

apply_test_() ->
    {ok, C} = sgte:compile("foo $apply bar myVar$ baz"),
    ?_assert(C =:= [<<"foo ">>, {apply, {[bar], [myVar]}, 1}, <<" baz">>]).

map_test_() ->
    {ok, C} = sgte:compile("foo $map bar varList$ baz"),
    ?_assert(C =:= [<<"foo ">>, {map, {[bar], [varList]}, 1}, <<" baz">>]).

mmap_test_() ->
    {ok, C} = sgte:compile("foo $mmap bar baz varList$"),
    ?_assert(C =:= [<<"foo ">>, {mmap, {[bar, baz], [varList]}, 1}]).

mapl_test_() ->
    {ok, C} = sgte:compile("foo $mapl    bar varList$"),
    ?_assert(C =:= [<<"foo ">>, {mapl, {[bar], [varList]}, 1}]).

mapj_test_() ->
    {ok, C} = sgte:compile("foo $mapj bar varList separator$"),
    ?_assert(C =:= [<<"foo ">>, {mapj, {[bar], [varList], [separator]}, 1}]).

inline_map_test_() ->
    {ok, C} = sgte:compile("foo $map:{template: $attr$} values$"),
    Result = [<<"foo ">>, {imap, {[[<<"template: ">>, {attribute, [attr], 1}]], [values]}, 1}],
    ?_assert(C =:= Result).

join_test_() ->
    {ok, C} = sgte:compile("foo $join:{separator} values$"),
    ?_assert(C =:= [<<"foo ">>, {join, {"separator", [values]}, 1}]).

if_test_() ->
    {ok, C} = sgte:compile(simple_if()),
    Result = [<<"Start ">>, {ift, {{attribute, [test, flag]}, [<<"then branch">>], [<<"else branch">>]}, 1}],
    ?_assert(C =:= Result).


%%--------------------
%%
%% Internal functions
%%
%%--------------------
%% Template String
simple_if() ->
    "Start $if test.flag$" ++
	"then branch" ++
        "$else$" ++
	"else branch"++
	"$end if$".

