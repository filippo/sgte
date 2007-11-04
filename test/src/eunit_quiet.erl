-module(eunit_quiet).

-include_lib("eunit/include/eunit.hrl").

%%--------------------
%%
%% Tests
%%
%%--------------------
%%
%% Render Test
%%
string_test_() ->
    Str = "This is a test:\n" ++
	"$testFun()$ followed by $testData$ and unicode chars àèìòù",
    {ok, Compiled} = sgte:compile(Str),
    Res1 = sgte:render_str(Compiled, []),
    Res2 = sgte:render_str(Compiled, [], [quiet]),
    ResultStr1 = "This is a test:\n" ++
	"[SGTE Warning: template: attribute - key 'testFun()' not found on line 2] followed by [SGTE Warning: template: attribute - key testData not found on line 2] and unicode chars àèìòù",
    ResultStr2 = "This is a test:\n" ++
	" followed by  and unicode chars àèìòù",
    [?_assert(Res1 =:= ResultStr1), 
     ?_assert(Res2 =:= ResultStr2)].

include_test_() ->
    {ok, C1} = sgte:compile("foo $include tmpl$ baz"),
    Res1 = sgte:render_str(C1, [], [quiet]),
    Res2 = sgte:render_str(C1, []),
    ResultStr1 = "foo  baz",
    ResultStr2 = "foo [SGTE Warning: template: include - key tmpl not found on line 1] baz",
    [?_assert(Res1 =:= ResultStr1),
     ?_assert(Res2 =:= ResultStr2)].

apply_test_() ->
    {ok, C} = sgte:compile("foo $apply second myList$ baz"),
    Res = sgte:render_str(C, [], [quiet]),
    ResultStr = "foo  baz",
    ?_assert(Res =:= ResultStr).

simpleif_test_() ->
    {ok, C} = sgte:compile(simple_if()),
    DThen = [],
    DElse = [],
    RThen = sgte:render_str(C, DThen),
    RElse = sgte:render_str(C, DElse, [quiet]),
    [?_assert(RThen =:= "Start [SGTE Warning: template: ift - key test not found on line 1]"),
     ?_assert(RElse =:= "Start else branch")].

if_test_() ->
    {ok, Compiled} = sgte:compile(if_string()),
    Data1 = [{testNames, true}],
    Data2 = [],
    Res1 = sgte:render_str(Compiled, Data1, [quiet]),
    Res2 = sgte:render_str(Compiled, Data2, [quiet]),
    [?_assert(Res1 =:= "Hello! Some Mountains:  Bye Bye."),
     ?_assert(Res2 =:= "Hello!  Bye Bye.")].    
    
fif_test_() ->
    {ok, Compiled} = sgte:compile(if_string()),
    NameL = mountainList(),
    Data = [{testNames, check_names(NameL)}],
    Res = sgte:render_str(Compiled, Data, [quiet]),
    ?_assert(Res =:= "Hello! Some Mountains:  Bye Bye.").

fif2_test_() ->
    {ok, Compiled} = sgte:compile(if_string()),
    D1 = dict:new(),
    D2 = dict:store('testNames', check_names([]), D1),
    Res = sgte:render_str(Compiled, D2, [quiet]),
    ?_assert(Res =:= "Hello!  Bye Bye.").

nested_fif_test_() ->
    {ok, Compiled} = sgte:compile(nested_if_string()),
    NameL = mountainList(),
    D1 = dict:new(),
    D2 = dict:store('testNames', check_names(NameL), D1),
    Res = sgte:render_str(Compiled, D2, [quiet]),
    Res2 = sgte:render_str(Compiled, [], [quiet]),
    [?_assert(Res =:= "Some Mountains: "),
     ?_assert(Res2 =:= "")].

% test callable attribute
fun_test_() ->
    {ok, CF} = sgte:compile(tmpl_fun()),
    Res = sgte:render_str(CF, [{foo, "foooo"}], [quiet]),
    ?_assert(Res =:= "aaaa  bbb").

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
