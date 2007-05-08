-module(sgte_test_quiet).

-export([test_string/0, test_include/0, test_apply/0]).
-export([test_simpleif/0, test_simpleif_no_test/0]).
-export([test_fif/0, test_fif2/0, test_nested_fif/0, test_if/0]).
-export([test_fun/0, test_file/0]).


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
    {ok, C1} = sgte:compile("bar"),
    {ok, C2} = sgte:compile("foo $include tmpl$ baz"),
    Res1 = sgte:render(C2, [], [quiet]),
    Res2 = sgte:render(C2, []),
    ResultStr1 = "foo  baz",
    ResultStr2 = "foo [SGTE Warning: template: include - key tmpl not found on line 1] baz",
    [sgeunit:assert_equal(Res1, ResultStr1),
     sgeunit:assert_equal(Res2, ResultStr2)].

test_apply() ->
    F = fun(L) -> lists:nth(2, L) end,
    {ok, C} = sgte:compile("foo $apply second myList$ baz"),
    Res = sgte:render(C, [{second, F}, {myList, ["1", "2", "3"]}]),
    ResultStr = "foo 2 baz",
    sgeunit:assert_equal(Res, ResultStr).

test_simpleif() ->
    {ok, C} = sgte:compile(simple_if()),
    DThen = [{test, true}],
    DElse = [{test, false}],
    RThen = sgte:render(C, DThen),
    RElse = sgte:render(C, DElse),
    [sgeunit:assert_equal(RThen, "Start then branch"),
     sgeunit:assert_equal(RElse, "Start else branch")].

test_simpleif_no_test() ->
    {ok, C} = sgte:compile(simple_if()),
    RElse = sgte:render(C, []),
    sgeunit:assert_equal(RElse, "Start [SGTE Warning: template: ift - key test not found on line 1]").

test_if() ->
    {ok, Compiled} = sgte:compile(if_string()),
    NameL = mountainList(),
    Data1 = [{testNames, true},
	    {nameList, NameL}],
    Data2 = [{testNames, false},
	    {noName, fun no_name/1}],
    Res1 = sgte:render(Compiled, Data1),
    Res2 = sgte:render(Compiled, Data2),
    [sgeunit:assert_equal(Res1, "Hello! Some Mountains: Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio Bye Bye."),
     sgeunit:assert_equal(Res2, "Hello! No Name Found Bye Bye.")].    
    
test_fif() ->
    {ok, Compiled} = sgte:compile(if_string()),
    NameL = mountainList(),
    Data = [{testNames, check_names(NameL)},
	    {noName, fun no_name/1},
	    {nameList, NameL}],
    Res = sgte:render(Compiled, Data),
    sgeunit:assert_equal(Res, "Hello! Some Mountains: Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio Bye Bye.").

test_fif2() ->
    {ok, Compiled} = sgte:compile(if_string()),
    D1 = dict:new(),
    D2 = dict:store('testNames', check_names([]), D1),
    D3 = dict:store('noName', fun no_name/1, D2),
    D4 = dict:store('nameList', mountainList(), D3),
    Res = sgte:render(Compiled, D4),
    sgeunit:assert_equal(Res, "Hello! No Name Found Bye Bye.").

test_nested_fif() ->
    {ok, Compiled} = sgte:compile(nested_if_string()),
    NameL = mountainList(),
    D1 = dict:new(),
    D2 = dict:store('testNames', check_names(NameL), D1),
    D3 = dict:store('noName', fun no_name/1, D2),
    D4 = dict:store('nameList', NameL, D3),
    Res = sgte:render(Compiled, D4),
    sgeunit:assert_equal(Res, "Some Mountains: Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio").

% test callable attribute
test_fun() ->
    MyF = fun(Data) ->
		  {ok, V} = dict:find(foo, Data),
		  "TEST: " ++ V
	  end,
    {ok, CF} = sgte:compile(tmpl_fun()),
    Res = sgte:render(CF, [{foo, "foooo"}, {callme, MyF}]),
    sgeunit:assert_equal(Res, "aaaa TEST: foooo bbb").

%test on a non existent file
test_file() ->
    Res = sgte:compile_file("myfile.tmpl"),
    sgeunit:assert_equal(Res, {error, enoent}).


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

no_name(_Foo) ->
    "No Name Found".

check_names(NameList) ->
    length(NameList) > 0.


tmpl_fun() ->
    "aaaa $callme$ bbb".


%% Test Data
mountainList() ->
    ["Monte Bianco", "Cerro Torre", "Mt. Everest", "Catinaccio"].
