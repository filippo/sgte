-module(eunit_render_types).

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
	"$an_atom$, $an_integer$, $a_float$ and $a_binary$.",
    {ok, Compiled} = sgte:compile(Str),
    Res = sgte:render_str(Compiled, data()),
    ResultStr = "This is a test:\n" ++
	"foo, 123, 9.86999999999999921840e+00 and foo, bar, baz.",
    %% error test
    Str2 = "This is a test:\n" ++
	"$testAttr1$ followed by $testAttr2$ and unicode chars àèìòù",
    {ok, Compiled2} = sgte:compile(Str2),
    Res2 = sgte:render_str(Compiled2, []),
    ResultStr2 = "This is a test:\n" ++
	"[SGTE Warning: template: attribute - key testAttr1 not found on line 2] followed by [SGTE Warning: template: attribute - key testAttr2 not found on line 2] and unicode chars àèìòù",
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
    Res = sgte:render_str(C, [{second, F}, {myList, [1, 2, 3]}]),
    ResultStr = "foo 2 baz",
    ?_assert(Res =:= ResultStr).

% test callable attribute
fun_test_() ->
    MyF = fun(Data) ->
		  {ok, V} = dict:find(foo, Data),
		  "TEST: " ++ V
	  end,
    {ok, CF} = sgte:compile(tmpl_fun()),
    Res = sgte:render_str(CF, [{foo, "foooo"}, {callme, MyF}]),
    ?_assert(Res =:= "aaaa TEST: foooo bbb").

map_test_() ->
    {ok, Map} = sgte:compile("$map:{- $el$\n} values$"),
    Data = [{values, [{el, foo}, {el, bar}, {el, baz}]}],
    Res = sgte:render_str(Map, Data),
    Data1 = [{values, [{el, 1}, {el, 2}, {el, 3}]}],
    Res1 = sgte:render_str(Map, Data1),
    [?_assert(Res =:= "- foo\n- bar\n- baz\n"),
     ?_assert(Res1 =:= "- 1\n- 2\n- 3\n")].

mapj_test_() ->
    {ok, RowTmpl} = sgte:compile("- $el$"),
    {ok, Separator} = sgte:compile(", \n"),
    {ok, MapJ} = sgte:compile("$mapj row values sep$"),
    Data = [{row, RowTmpl}, {sep, Separator}, 
	    {values, [{el, foo}, {el, bar}, {el, baz}]}],
    Res = sgte:render_str(MapJ, Data),
    Rendered = "- foo, \n- bar, \n- baz",
    ?_assert(Res =:= Rendered).

tmpl_fun() ->
    "aaaa $callme$ bbb".

%% Test Data
data() ->
    D1 = dict:new(),
    D2 = dict:store('an_atom', foo, D1),
    D3 = dict:store('an_integer', 123, D2),
    D4 = dict:store('a_float', 9.87, D3),
    dict:store('a_binary', <<"foo, bar, baz">>, D4).
