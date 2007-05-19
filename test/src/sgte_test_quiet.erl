-module(sgte_test_quiet).

-export([test_string/0, test_include/0, test_apply/0]).
-export([test_simpleif/0]).
-export([test_fif/0, test_fif2/0, test_nested_fif/0, test_if/0]).
-export([test_fun/0]).


%%--------------------
%%
%% Tests
%%
%%--------------------
%%
%% Render Test
%%
test_string() ->
    Str = "This is a test:\n" ++
	"$testFun()$ followed by $testData$ and unicode chars àèìòù",
    {ok, Compiled} = sgte:compile(Str),
    Res1 = sgte:render(Compiled, []),
    Res2 = sgte:render(Compiled, [], [quiet]),
    ResultStr1 = "This is a test:\n" ++
	"[SGTE Warning: template: attribute - key 'testFun()' not found on line 2] followed by [SGTE Warning: template: attribute - key testData not found on line 2] and unicode chars àèìòù",
    ResultStr2 = "This is a test:\n" ++
	" followed by  and unicode chars àèìòù",
    [sgeunit:assert_equal(Res1, ResultStr1), 
     sgeunit:assert_equal(Res2, ResultStr2)].

test_include() ->
    {ok, C1} = sgte:compile("foo $include tmpl$ baz"),
    Res1 = sgte:render(C1, [], [quiet]),
    Res2 = sgte:render(C1, []),
    ResultStr1 = "foo  baz",
    ResultStr2 = "foo [SGTE Warning: template: include - key tmpl not found on line 1] baz",
    [sgeunit:assert_equal(Res1, ResultStr1),
     sgeunit:assert_equal(Res2, ResultStr2)].

test_apply() ->
    {ok, C} = sgte:compile("foo $apply second myList$ baz"),
    Res = sgte:render(C, [], [quiet]),
    ResultStr = "foo  baz",
    sgeunit:assert_equal(Res, ResultStr).

test_simpleif() ->
    {ok, C} = sgte:compile(simple_if()),
    DThen = [],
    DElse = [],
    RThen = sgte:render(C, DThen),
    RElse = sgte:render(C, DElse, [quiet]),
    [sgeunit:assert_equal(RThen, "Start [SGTE Warning: template: ift - key test not found on line 1]"),
     sgeunit:assert_equal(RElse, "Start else branch")].

test_if() ->
    {ok, Compiled} = sgte:compile(if_string()),
    Data1 = [{testNames, true}],
    Data2 = [],
    Res1 = sgte:render(Compiled, Data1, [quiet]),
    Res2 = sgte:render(Compiled, Data2, [quiet]),
    [sgeunit:assert_equal(Res1, "Hello! Some Mountains:  Bye Bye."),
     sgeunit:assert_equal(Res2, "Hello!  Bye Bye.")].    
    
test_fif() ->
    {ok, Compiled} = sgte:compile(if_string()),
    NameL = mountainList(),
    Data = [{testNames, check_names(NameL)}],
    Res = sgte:render(Compiled, Data, [quiet]),
    sgeunit:assert_equal(Res, "Hello! Some Mountains:  Bye Bye.").

test_fif2() ->
    {ok, Compiled} = sgte:compile(if_string()),
    D1 = dict:new(),
    D2 = dict:store('testNames', check_names([]), D1),
    Res = sgte:render(Compiled, D2, [quiet]),
    sgeunit:assert_equal(Res, "Hello!  Bye Bye.").

test_nested_fif() ->
    {ok, Compiled} = sgte:compile(nested_if_string()),
    NameL = mountainList(),
    D1 = dict:new(),
    D2 = dict:store('testNames', check_names(NameL), D1),
    Res = sgte:render(Compiled, D2, [quiet]),
    Res2 = sgte:render(Compiled, [], [quiet]),
    [sgeunit:assert_equal(Res, "Some Mountains: "),
     sgeunit:assert_equal(Res2, "")].

% test callable attribute
test_fun() ->
    {ok, CF} = sgte:compile(tmpl_fun()),
    Res = sgte:render(CF, [{foo, "foooo"}], [quiet]),
    sgeunit:assert_equal(Res, "aaaa  bbb").

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

if_string() ->
    "Hello! $if testNames$" ++
	"Some Mountains: $join:{, } nameList$" ++
        "$else$" ++
	"$noName$$end if$" ++ " Bye Bye.".

nested_if_string() ->
    "$if testNames$" ++
	"Some Mountains: $if testNames$$join:{, } nameList$$end if$" ++
        "$else$" ++
	"$noName$$end if$".

check_names(NameList) ->
    length(NameList) > 0.


tmpl_fun() ->
    "aaaa $callme$ bbb".


%% Test Data
mountainList() ->
    ["Monte Bianco", "Cerro Torre", "Mt. Everest", "Catinaccio"].
