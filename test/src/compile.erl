-module(sgte_test_compile).

-export([test_compile_attr/0, test_compile_include/0, test_compile_apply/0]).
-export([test_compile_map/0, test_compile_mapl/0]).
-export([test_compile_mapj/0, test_compile_mmap/0]).
-export([test_compile_inline_map/0, test_compile_if/0]).

%%--------------------
%%
%% Tests
%%
%%--------------------
%%
%% Compile Test
%%
test_compile_attr() ->
    Str = "foo $bar$ baz",
    {ok, Compiled} = sgte:compile(Str),
    sgeunit:assert_equal(Compiled, "foo " ++ [{attribute, bar, 1}] ++ " baz").

test_compile_include() ->
    {ok, C} = sgte:compile("foo $include tmpl$ baz"),
    sgeunit:assert_equal(C, "foo " ++ [{include, tmpl, 1}] ++ " baz").

test_compile_apply() ->
    {ok, C} = sgte:compile("foo $apply bar myVar$ baz"),
    sgeunit:assert_equal(C, "foo " ++ [{apply, {bar, myVar}, 1}] ++ " baz").

test_compile_map() ->
    {ok, C} = sgte:compile("foo $map bar varList$ baz"),
    sgeunit:assert_equal(C, "foo " ++ [{map, {bar, varList}, 1}] ++ " baz").

test_compile_mmap() ->
    {ok, C} = sgte:compile("foo $mmap bar baz varList$"),
    sgeunit:assert_equal(C, "foo " ++ [{mmap, {[bar, baz], varList}, 1}]).

test_compile_mapl() ->
    {ok, C} = sgte:compile("foo $mapl bar varList$"),
    sgeunit:assert_equal(C, "foo " ++ [{mapl, {bar, varList}, 1}]).

test_compile_mapj() ->
    {ok, C} = sgte:compile("foo $mapj bar varList separator$"),
    sgeunit:assert_equal(C, "foo " ++ [{mapj, {bar, varList, separator}, 1}]).

test_compile_inline_map() ->
    sgeunit:fail().

test_compile_if() ->
    sgeunit:fail().
%%     {ok, C} = sgte:compile(simple_if()),
%%     Result = "Start " ++ [{ift, {{attribute, test}, "then branch", "else branch"}}],
%%     sgeunit:assert_equal(C, Result).


%%--------------------
%%
%% Internal functions
%%
%%--------------------
%% Template String
simple_if() ->
    "Start $if test$" ++
	"then branch" ++
        "$else$" ++
	"else branch"++
	"$end if$".

