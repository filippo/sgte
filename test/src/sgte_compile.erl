-module(sgte_compile).

-export([test_attr/0, test_attr_multiline/0]).
-export([test_include/0, test_include_multiline/0]).
-export([test_apply/0]).
-export([test_map/0, test_mapl/0]).
-export([test_mapj/0, test_mmap/0]).
-export([test_inline_map/0, test_if/0]).

%%--------------------
%%
%% Tests
%%
%%--------------------
test_attr() ->
    Str = "foo $bar$ baz",
    {ok, Compiled} = sgte:compile(Str),
    sgeunit:assert_equal(Compiled, "foo " ++ [{attribute, bar, 1}] ++ " baz").

test_attr_multiline() ->
    Str = "foo 
$bar
$
 baz",
    {ok, Compiled} = sgte:compile(Str),
    sgeunit:assert_equal(Compiled, "foo \n" ++ [{attribute, bar, 2}] ++ "\n baz").

test_include() ->
    {ok, C} = sgte:compile("foo $include tmpl$ baz"),
    sgeunit:assert_equal(C, "foo " ++ [{include, tmpl, 1}] ++ " baz").

test_include_multiline() ->
    Str = "foo $include
tmpl$ baz",
    {ok, C} = sgte:compile(Str),
    sgeunit:assert_equal(C, "foo " ++ [{include, tmpl, 1}] ++ " baz").

test_apply() ->
    {ok, C} = sgte:compile("foo $apply bar myVar$ baz"),
    sgeunit:assert_equal(C, "foo " ++ [{apply, {bar, myVar}, 1}] ++ " baz").

test_map() ->
    {ok, C} = sgte:compile("foo $map bar varList$ baz"),
    sgeunit:assert_equal(C, "foo " ++ [{map, {bar, varList}, 1}] ++ " baz").

test_mmap() ->
    {ok, C} = sgte:compile("foo $mmap bar baz varList$"),
    sgeunit:assert_equal(C, "foo " ++ [{mmap, {[bar, baz], varList}, 1}]).

test_mapl() ->
    {ok, C} = sgte:compile("foo $mapl    bar varList$"),
    sgeunit:assert_equal(C, "foo " ++ [{mapl, {bar, varList}, 1}]).

test_mapj() ->
    {ok, C} = sgte:compile("foo $mapj bar varList separator$"),
    sgeunit:assert_equal(C, "foo " ++ [{mapj, {bar, varList, separator}, 1}]).

test_inline_map() ->
    sgeunit:fail().

test_if() ->
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

