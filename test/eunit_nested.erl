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
    Res = sgte:render_str(C, [{foo, [{bar, [{baz, "foo, bar and baz"}]}]}]),
    ResultStr = "foo, bar and baz",
    ?_assert(Res =:= ResultStr).

record_test_() ->
    Rec = #test_rec{col1="foo", col2="bar", col3="baz"},
    {ok, C} = sgte:compile("$test_rec.col1$, $test_rec.col2$ and $test_rec.col3$"),
    Res = sgte:render_str(C, [sgte:rec_to_name_kv(Rec, record_info(fields, test_rec))]),
    ResultStr = "foo, bar and baz",
    ?_assert(Res =:= ResultStr).

nested_record_test_() ->
    Rec2 = sgte:rec_to_kv(#test_rec{col1="foo", col2="bar", col3="baz"},
                          record_info(fields, test_rec)),
    Rec = #test_rec{col1=Rec2},
    {ok, C} = sgte:compile("$test_rec.col1.col1$, $test_rec.col1.col2$ and $test_rec.col1.col3$"),
    Res = sgte:render_str(C, [sgte:rec_to_name_kv(Rec, record_info(fields, test_rec))]),
    ResultStr = "foo, bar and baz",
    ?_assert(Res =:= ResultStr).
    
include_test_() ->
    {ok, C1} = sgte:compile("bar"),
    {ok, C2} = sgte:compile("foo $include tmpl.bar$ baz"),
    Res = sgte:render_str(C2, [{tmpl, [{bar, C1}]}]),
    ResultStr = "foo bar baz",
    ?_assert(Res =:= ResultStr).

apply_test_() ->
    F = fun(L) -> lists:nth(2, L) end,
    {ok, C} = sgte:compile("foo $apply list.second myList$ baz"),
    Res = sgte:render_str(C, [{list, [{second, F}]}, 
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
    RThen  = sgte:render_str(C, DThen),
    RElse1 = sgte:render_str(C, DElse1),
    RElse2 = sgte:render_str(C, DElse2),
    RElse3 = sgte:render_str(C, DElse3),
    RElse4 = sgte:render_str(C, DElse4),
    RElse5 = sgte:render_str(C, DElse5),
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
    RElse = sgte:render_str(C, [], [quiet]),
    ?_assert(RElse =:= "Start else branch").

if_test_() ->
    {ok, Compiled} = sgte:compile(if_string()),
    NameL = mountainList(),
    Data1 = [{test, [{names, true}]},
	    {nameList, NameL}],
    Data2 = [{test, [{names, false}]},
	    {noName, fun no_name/1}],
    Res1 = sgte:render_str(Compiled, Data1),
    Res2 = sgte:render_str(Compiled, Data2),
    [?_assert(Res1 =:= "Hello! Some Mountains: Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio Bye Bye."),
     ?_assert(Res2 =:= "Hello! No Name Found Bye Bye.")].    

js_support_test_() ->
    {ok, CF} = sgte:compile("$('someId') and a $nested.attr$ and $('anotherId')"),
    Res = sgte:render_str(CF, [{nested, [{attr, "nested attribute"}]}]),
    ?_assert(Res =:= "$('someId') and a nested attribute and $('anotherId')").


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
    "Hello! $if test.names$" ++
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
