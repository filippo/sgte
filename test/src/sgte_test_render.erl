-module(sgte_test_render).

-export([test_string/0, test_string_err/0, test_include/0, test_apply/0]).
-export([test_simpleif/0, test_fif/0, test_fif2/0, test_nested_fif/0, test_if/0]).
-export([test_map/0, test_map_on_empty_list/0, test_mmap/0]).
-export([test_imap/0,test_imap2/0, test_imap_name_place/0, test_fun/0]).
-export([test_imap_js/0, test_imap_comma/0]).
-export([test_file/0]).


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
	"$testFun()$ followed by $testData$ and unicode characters  àèìòù",
    {ok, Compiled} = sgte:compile(Str),
    Res = sgte:render(Compiled, data()),
    ResultStr = "This is a test:\n" ++
	"foo, bar, baz followed by my test data with unicode characters: àèìòù and unicode characters  àèìòù",
    sgeunit:assert_equal(Res, ResultStr).

test_string_err() ->
    Str = "This is a test:\n" ++
	"$testFun()$ followed by $testData$ and unicode chars àèìòù",
    {ok, Compiled} = sgte:compile(Str),
    Res = sgte:render(Compiled, []),
    ResultStr = "This is a test:\n" ++
	"[SGTE Error: template: attribute - key 'testFun()' not found on line 2] followed by [SGTE Error: template: attribute - key testData not found on line 2] and unicode chars àèìòù",
    sgeunit:assert_equal(Res, ResultStr).

test_include() ->
    {ok, C1} = sgte:compile("bar"),
    {ok, C2} = sgte:compile("foo $include tmpl$ baz"),
    Res = sgte:render(C2, [{tmpl, C1}]),
    ResultStr = "foo bar baz",
    sgeunit:assert_equal(Res, ResultStr).

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

test_map() ->
    {ok, PrintM} = sgte:compile(print_mountain()),
    {ok, PrintMList} = sgte:compile(print_mountains()),
    Data = [{nameList, mountains()}, {printMountain, PrintM}],
    Res = sgte:render(PrintMList, Data),
    Rendered = "<ul><li><b>Monte Bianco</b></li><li><b>Cerro Torre</b></li><li><b>Mt. Everest</b></li><li><b>Catinaccio</b></li></ul>",
    sgeunit:assert_equal(Res, Rendered).

test_map_on_empty_list() ->
    {ok, PrintM} = sgte:compile(print_mountain()),
    {ok, PrintMList} = sgte:compile(print_mountains()),
    Data = [{nameList, empty()}, {printMountain, PrintM}],
    Res = sgte:render(PrintMList, Data),
    Rendered = "<ul></ul>",
    sgeunit:assert_equal(Res, Rendered).

test_mmap() ->
    {ok, PrintMList} = sgte:compile(print_mmap()),
    {ok, R1} = sgte:compile(row1()),
    {ok, R2} = sgte:compile(row2()),
    Data = [{nameList, mountains()}, 
	    {row1, R1}, 
	    {row2, R2}],
    Res = sgte:render(PrintMList, Data),
    Rendered = "<ul>"++
	"<li class=\"riga1\"><b>Monte Bianco</b></li>"++
	"<li class=\"riga2\"><b>Cerro Torre</b></li>"++
	"<li class=\"riga1\"><b>Mt. Everest</b></li>"++
	"<li class=\"riga2\"><b>Catinaccio</b></li>"++
	"</ul>",
    sgeunit:assert_equal(Res, Rendered).

test_imap() ->
    {ok, PrintMList} = sgte:compile(print_inline_mountains()),
    Data = [{nameList, mountains()}, {myClass, "listItem"}],
    Res = sgte:render(PrintMList, Data),
    Rendered = "<ul>"++
	"<li class=\"listItem\"><b>Monte Bianco</b></li>"++
	"<li class=\"listItem\"><b>Cerro Torre</b></li>"++
	"<li class=\"listItem\"><b>Mt. Everest</b></li>"++
	"<li class=\"listItem\"><b>Catinaccio</b></li>"++
	"</ul>",
    sgeunit:assert_equal(Res, Rendered).

test_imap_name_place() ->
    {ok, PrintMList} = sgte:compile(print_inline_mountain_place()),
    Data = [{nameList, mountains2()}],
    Res = sgte:render(PrintMList, Data),
    Rendered = "<ul>"++
	"<li><b>Monte Bianco</b> - Alps</li>"++
	"<li><b>Cerro Torre</b> - Patagonia</li>"++
	"<li><b>Mt. Everest</b> - Himalaya</li>"++
	"<li><b>Catinaccio</b> - Dolomites</li>"++
	"</ul>",
    sgeunit:assert_equal(Res, Rendered).

test_imap2() ->
    {ok, PrintMList} = sgte:compile(print_inline_mountains2()),
    Data = [{nameList, mountains()}, {myClass, "listItem"},  {myClass2, "listItem2"}],
    Res = sgte:render(PrintMList, Data),
    Rendered = "<ul>\n"++
	"<li class=\"listItem\"><b>Monte Bianco</b></li>\n"++
	"<li class=\"listItem2\"><b>Cerro Torre</b></li>\n"++
	"<li class=\"listItem\"><b>Mt. Everest</b></li>\n"++
	"<li class=\"listItem2\"><b>Catinaccio</b></li>\n"++
	"</ul>",
    sgeunit:assert_equal(Res, Rendered).

test_imap_js() ->
    {ok, C} = sgte:compile(imap_js()),
    Rendered = sgte:render(C, [{owners, 
				   [{owner, "tobbe"}, 
				    {owner, "magnus"}
				   ]}]
			     ),
    Result = "\"#tobbe\": function(t) {save_owner(\"tobbe\",t.id);}, "++
	"\"#magnus\": function(t) {save_owner(\"magnus\",t.id);}, ",
    sgeunit:assert_equal(Rendered, Result).

test_imap_comma() ->
    {ok, C} = sgte:compile(imap_comma()),
    Rendered = sgte:render(C, [{attrList, 
				   [{attr, "First Attribute"}, 
				    {attr, "and the Second"}
				   ]}]
			     ),
    Result = "First Attribute, and the Second, ",
    sgeunit:assert_equal(Rendered, Result).

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
	"Some Mountains: $join \", \" nameList$" ++
        "$else$" ++
	"$noName$$end if$" ++ " Bye Bye.".

nested_if_string() ->
    "$if testNames$" ++
	"Some Mountains: $if testNames$$join \", \" nameList$$end if$" ++
        "$else$" ++
	"$noName$$end if$".

no_name(_Foo) ->
    "No Name Found".

check_names(NameList) ->
    length(NameList) > 0.

print_mountains() ->
    "<ul>" ++
	"$map printMountain nameList$" ++
    "</ul>".
print_mountain() ->
    "<li><b>$mountain$</b></li>".
print_inline_mountains() ->
    "<ul>" ++
	"$map:{<li class=\"$myClass$\"><b>$mountain$</b></li>} nameList$" ++
    "</ul>".
print_inline_mountains2() ->
    "<ul>\n" ++
	"$map:{<li class=\"$myClass$\"><b>$mountain$</b></li>,\n"++
	"<li class=\"$myClass2$\"><b>$mountain$</b></li>\n} nameList$" ++
	"</ul>".
print_mmap() ->
    "<ul>" ++
	"$mmap row1 row2 nameList$" ++
    "</ul>".
row1() ->
    "<li class=\"riga1\"><b>$mountain$</b></li>".
row2() ->
    "<li class=\"riga2\"><b>$mountain$</b></li>".

print_inline_mountain_place() ->
    "<ul>" ++
	"$map:{<li><b>$mountain$</b> - $place$</li>} nameList$" ++
    "</ul>".

tmpl_fun() ->
    "aaaa $callme$ bbb".


imap_js() ->
    "$map:{\"#$owner$\": function(t) {save_owner(\"$owner$\"\\,t.id);}\\, } owners$".

imap_comma() ->
    "$map:{$attr$\\, } attrList$".

%% Test Data
data() ->
    D1 = dict:new(),
    D2 = dict:store('testFun()', "foo, bar, baz", D1),
    dict:store('testData', "my test data with unicode characters: àèìòù", D2).

empty() ->
    [].

mountains() ->
    [{mountain, "Monte Bianco"}, {mountain, "Cerro Torre"}, {mountain, "Mt. Everest"}, {mountain, "Catinaccio"}].

mountains2() ->
    [[{mountain, "Monte Bianco"}, {place, "Alps"}], 
     [{mountain, "Cerro Torre"}, {place, "Patagonia"}], 
     [{mountain, "Mt. Everest"}, {place, "Himalaya"}],
     [{mountain, "Catinaccio"}, {place, "Dolomites"}]].

mountainList() ->
    ["Monte Bianco", "Cerro Torre", "Mt. Everest", "Catinaccio"].
