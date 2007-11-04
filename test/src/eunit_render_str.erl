-module(eunit_render_str).

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
	"$testFun()$ followed by $testData$ and unicode characters  àèìòù",
    {ok, Compiled} = sgte:compile(Str),
    Res = sgte:render_str(Compiled, data()),
    ResultStr = "This is a test:\n" ++
        "foo, bar, baz" ++
        " followed by " ++
        "my test data with unicode characters: àèìòù" ++
        " and unicode characters  àèìòù",
    %% error test
    Str2 = "This is a test:\n" ++
	"$testFun()$ followed by $testData$ and unicode chars àèìòù",
    {ok, Compiled2} = sgte:compile(Str2),
    Res2 = sgte:render_str(Compiled2, []),
    ResultStr2 = "This is a test:\n"++
        "[SGTE Warning: template: attribute - key 'testFun()' not found on line 2]"++
        " followed by "++
        "[SGTE Warning: template: attribute - key testData not found on line 2]"++
        " and unicode chars àèìòù",
    [?_assert(Res =:= ResultStr),
     ?_assert(Res2 =:= ResultStr2)].

include_test_() ->
    {ok, C1} = sgte:compile("bar"),
    {ok, C2} = sgte:compile("foo $include tmpl$ baz"),
    Res = sgte:render_str(C2, [{tmpl, C1}]),
    ResultStr = "foo bar baz",
    ?_assert(Res =:= ResultStr).

apply_test_() ->
    F = fun(L) -> lists:nth(2, L) end,
    {ok, C} = sgte:compile("foo $apply second myList$ baz"),
    Res = sgte:render_str(C, [{second, F}, {myList, ["1", "2", "3"]}]),
    ResultStr = "foo 2 baz",
    ?_assert(Res =:= ResultStr).

simpleif_test_() ->
    {ok, C} = sgte:compile(simple_if()),
    DThen = [{test, true}],
    DElse1 = [{test, false}],
    DElse2 = [{test, []}],
    DElse3 = [{test, ""}],
    DElse4 = [{test, {}}],
    DElse5 = [{test, 0}],
    RThen  = sgte:render_str(C, DThen),
    RElse1 = sgte:render_str(C, DElse1),
    RElse2 = sgte:render_str(C, DElse2),
    RElse3 = sgte:render_str(C, DElse3),
    RElse4 = sgte:render_str(C, DElse4),
    RElse5 = sgte:render_str(C, DElse5),
    ThenStr = "Start then branch",
    ElseStr = "Start else branch",
    [?_assert(RThen  =:= ThenStr),
     ?_assert(RElse1 =:= ElseStr),
     ?_assert(RElse2 =:= ElseStr),
     ?_assert(RElse3 =:= ElseStr),
     ?_assert(RElse4 =:= ElseStr),
     ?_assert(RElse5 =:= ElseStr)].

simpleif_no_test_test_() ->
    {ok, C} = sgte:compile(simple_if()),
    RElse = sgte:render_str(C, [], [quiet]),
    ?_assert(RElse =:= "Start else branch").

if_test_() ->
    {ok, Compiled} = sgte:compile(if_string()),
    NameL = mountainList(),
    Data1 = [{testNames, true},
             {nameList, NameL}],
    Data2 = [{testNames, false},
             {noName, fun no_name/1}],
    Res1 = sgte:render_str(Compiled, Data1),
    Res2 = sgte:render_str(Compiled, Data2),
    [?_assert(Res1 =:= "Hello! Some Mountains: "++
              "Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio Bye Bye."),
     ?_assert(Res2 =:= "Hello! No Name Found Bye Bye.")].    
    
fif_test_() ->
    {ok, Compiled} = sgte:compile(if_string()),
    NameL = mountainList(),
    Data = [{testNames, check_names(NameL)},
	    {noName, fun no_name/1},
	    {nameList, NameL}],
    Res = sgte:render_str(Compiled, Data),
    {ok, Compiled2} = sgte:compile(if_string()),
    D1 = dict:new(),
    D2 = dict:store('testNames', check_names([]), D1),
    D3 = dict:store('noName', fun no_name/1, D2),
    D4 = dict:store('nameList', mountainList(), D3),
    Res2 = sgte:render_str(Compiled2, D4),
    [?_assert(Res =:= "Hello! Some Mountains: "++
              "Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio Bye Bye."),
     ?_assert(Res2 =:= "Hello! No Name Found Bye Bye.")].

nested_fif_test_() ->
    {ok, Compiled} = sgte:compile(nested_if_string()),
    NameL = mountainList(),
    D1 = dict:new(),
    D2 = dict:store('testNames', check_names(NameL), D1),
    D3 = dict:store('noName', fun no_name/1, D2),
    D4 = dict:store('nameList', NameL, D3),
    Res = sgte:render_str(Compiled, D4),
    ?_assert(Res =:= "Some Mountains: Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio").

join_test_() ->
    {ok, C} = sgte:compile("$join:{, } aList$"),
    R1 = sgte:render_str(C, [{aList, ["foo", "bar", "baz"]}]),
    R2 = sgte:render_str(C, [{aList, []}]),
    [?_assert(R1 =:= "foo, bar, baz"),
     ?_assert(R2 =:= "")].

% test callable attribute
fun_test_() ->
    MyF = fun(Data) ->
		  {ok, V} = dict:find(foo, Data),
		  "TEST: " ++ V
	  end,
    {ok, CF} = sgte:compile(tmpl_fun()),
    Res = sgte:render_str(CF, [{foo, "foooo"}, {callme, MyF}]),
    ?_assert(Res =:= "aaaa TEST: foooo bbb").

%test on a non existent file
file_test_() ->
    Res = sgte:compile_file("myfile.tmpl"),
    ?_assert(Res =:= {error, enoent}).

js_support_test_() ->
    {ok, CF} = sgte:compile("$('someId') and an $attr$ and $('anotherId')"),
    Res = sgte:render_str(CF, [{attr, "attribute"}]),
    ?_assert(Res =:= "$('someId') and an attribute and $('anotherId')").


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
data() ->
    D1 = dict:new(),
    D2 = dict:store('testFun()', "foo, bar, baz", D1),
    dict:store('testData', "my test data with unicode characters: àèìòù", D2).

mountainList() ->
    ["Monte Bianco", "Cerro Torre", "Mt. Everest", "Catinaccio"].
