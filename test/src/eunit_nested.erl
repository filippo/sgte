-module(eunit_nested).

-include_lib("eunit/include/eunit.hrl").

-record(test_rec, {col1, col2, col3}).

%%--------------------
%%
%% Tests
%%
%%--------------------
%%
%% Render Test
%%
multi_attr_test_() ->
    {ok, C} = sgte:compile("$foo.bar.baz$"),
    Res = sgte:render(C, [{foo, [{bar, [{baz, "foo, bar and baz"}]}]}]),
    ResultStr = "foo, bar and baz",
    ?_assert(Res =:= ResultStr).

record_test_() ->
    Rec = #test_rec{col1="foo", col2="bar", col3="baz"},
    {ok, C} = sgte:compile("$test_rec.col1$, $test_rec.col2$ and $test_rec.col3$"),
    Res = sgte:render(C, [sgte:rec_to_name_kv(Rec, record_info(fields, test_rec))]),
    ResultStr = "foo, bar and baz",
    ?_assert(Res =:= ResultStr).

nested_record_test_() ->
    Rec2 = sgte:rec_to_kv(#test_rec{col1="foo", col2="bar", col3="baz"},
                          record_info(fields, test_rec)),
    Rec = #test_rec{col1=Rec2},
    {ok, C} = sgte:compile("$test_rec.col1.col1$, $test_rec.col1.col2$ and $test_rec.col1.col3$"),
    Res = sgte:render(C, [sgte:rec_to_name_kv(Rec, record_info(fields, test_rec))]),
    ResultStr = "foo, bar and baz",
    ?_assert(Res =:= ResultStr).
    
include_test_() ->
    {ok, C1} = sgte:compile("bar"),
    {ok, C2} = sgte:compile("foo $include tmpl.bar$ baz"),
    Res = sgte:render(C2, [{tmpl, [{bar, C1}]}]),
    ResultStr = "foo bar baz",
    ?_assert(Res =:= ResultStr).

apply_test_() ->
    F = fun(L) -> lists:nth(2, L) end,
    {ok, C} = sgte:compile("foo $apply list.second myList$ baz"),
    Res = sgte:render(C, [{list, [{second, F}]}, 
                          {myList, ["1", "2", "3"]}]),
    ResultStr = "foo 2 baz",
    ?_assert(Res =:= ResultStr).

simpleif_test_() ->
    {ok, C} = sgte:compile(simple_if()),
    DThen =  [{test, [{flag, true}]}],
    DElse1 = [{test, [{flag, false}]}],
    DElse2 = [{test, [{flag, []}]}],
    DElse3 = [{test, [{flag, ""}]}],
    DElse4 = [{test, [{flag, {}}]}],
    DElse5 = [{test, [{flag, 0}]}],
    RThen  = sgte:render(C, DThen),
    RElse1 = sgte:render(C, DElse1),
    RElse2 = sgte:render(C, DElse2),
    RElse3 = sgte:render(C, DElse3),
    RElse4 = sgte:render(C, DElse4),
    RElse5 = sgte:render(C, DElse5),
    ThenStr = "Start then branch",
    ElseStr =  "Start else branch",
    [?_assertMatch(RThen, ThenStr),
     ?_assertMatch(RElse1, ElseStr),
     ?_assertMatch(RElse2, ElseStr),
     ?_assertMatch(RElse3, ElseStr),
     ?_assertMatch(RElse4, ElseStr),
     ?_assertMatch(RElse5, ElseStr)].

simpleif_no_test_test_() ->
    {ok, C} = sgte:compile(simple_if()),
    RElse = sgte:render(C, [], [quiet]),
    ?_assert(RElse =:= "Start else branch").

if_test_() ->
    {ok, Compiled} = sgte:compile(if_string()),
    NameL = mountainList(),
    Data1 = [{testNames, true},
	    {nameList, NameL}],
    Data2 = [{testNames, false},
	    {noName, fun no_name/1}],
    Res1 = sgte:render(Compiled, Data1),
    Res2 = sgte:render(Compiled, Data2),
    [?_assert(Res1 =:= "Hello! Some Mountains: Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio Bye Bye."),
     ?_assert(Res2 =:= "Hello! No Name Found Bye Bye.")].    
    
fif_test_() ->
    {ok, Compiled} = sgte:compile(if_string()),
    NameL = mountainList(),
    Data = [{testNames, check_names(NameL)},
	    {noName, fun no_name/1},
	    {nameList, NameL}],
    Res = sgte:render(Compiled, Data),
    {ok, Compiled2} = sgte:compile(if_string()),
    D1 = dict:new(),
    D2 = dict:store('testNames', check_names([]), D1),
    D3 = dict:store('noName', fun no_name/1, D2),
    D4 = dict:store('nameList', mountainList(), D3),
    Res2 = sgte:render(Compiled2, D4),
    [?_assert(Res =:=
              "Hello! Some Mountains: Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio Bye Bye."),
     ?_assert(Res2 =:= "Hello! No Name Found Bye Bye.")].

nested_fif_test_() ->
    {ok, Compiled} = sgte:compile(nested_if_string()),
    NameL = mountainList(),
    D1 = dict:new(),
    D2 = dict:store('testNames', check_names(NameL), D1),
    D3 = dict:store('noName', fun no_name/1, D2),
    D4 = dict:store('nameList', NameL, D3),
    Res = sgte:render(Compiled, D4),
    ?_assert(Res =:= "Some Mountains: Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio").

% test callable attribute
fun_test_() ->
    MyF = fun(Data) ->
		  {ok, V} = dict:find(foo, Data),
		  "TEST: " ++ V
	  end,
    {ok, CF} = sgte:compile(tmpl_fun()),
    Res = sgte:render(CF, [{foo, "foooo"}, {callme, MyF}]),
    ?_assert(Res =:= "aaaa TEST: foooo bbb").

%test on a non existent file
file_test_() ->
    Res = sgte:compile_file("myfile.tmpl"),
    ?_assert(Res =:= {error, enoent}).

js_support_test_() ->
    {ok, CF} = sgte:compile("$('someId') and an $attr$ and $('anotherId')"),
    Res = sgte:render(CF, [{attr, "attribute"}]),
    ?_assert(Res =:= "$('someId') and an attribute and $('anotherId')").


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
