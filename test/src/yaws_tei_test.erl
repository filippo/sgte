-module(yaws_tei_test).

-export([setup_test/0]).

-export([test_string/0, test_string_err/0, test_include/0, test_apply/0]).
-export([test_simpleif/0, test_fif/0, test_fif2/0, test_nested_fif/0, test_if/0]).
-export([test_map/0, test_map_on_empty_list/0, test_mmap/0]).
-export([test_imap/0,test_imap2/0, test_imap_name_place/0, test_fun/0]).
-export([test_file/0]).


%%--------------------
%%
%% Tests
%%
%%--------------------
setup_test() ->
    yaws_tei:start_link().

%%
%% Render Test
%%
test_string() ->
    Str = "This is a test:\n" ++
	"$testFun()$ followed by $testData$ and unicode characters  àèìòù",
    yaws_tei:register_template(test_str, Str, sgte),
    {ok, F} = yaws_tei:lookup(test_str),
    Res = F(data()),
    ResultStr = "This is a test:\n" ++
	"foo, bar, baz followed by my test data with unicode characters: àèìòù and unicode characters  àèìòù",
    sgeunit:assert_equal(Res, ResultStr).

test_string_err() ->
    Str = "This is a test:\n" ++
	"$testFun()$ followed by $testData$ and unicode chars àèìòù",
    yaws_tei:register_template(test_str, Str, sgte),
    {ok, F} = yaws_tei:lookup(test_str),
    Res = F([]),
    ResultStr = "This is a test:\n" ++
	"[SGTE Error: template: attribute - key 'testFun()' not found] followed by [SGTE Error: template: attribute - key testData not found] and unicode chars àèìòù",
    sgeunit:assert_equal(Res, ResultStr).

test_include() ->
    yaws_tei:register_template(bar, "bar", sgte),
    yaws_tei:register_template(foo, "foo $include tmpl$ baz", sgte),
    {ok, Included} = yaws_tei:lookup(bar),
    {ok, Foo} = yaws_tei:lookup(foo),
    Res = Foo([{tmpl, Included}]),
    ResultStr = "foo bar baz",
    sgeunit:assert_equal(Res, ResultStr).

test_apply() ->
    F = fun(L) -> lists:nth(2, L) end,
    yaws_tei:register_template(foo, "foo $apply second myList$ baz", sgte),
    {ok, R} = yaws_tei:lookup(foo),
    Res = R([{second, F}, {myList, ["1", "2", "3"]}]),
    ResultStr = "foo 2 baz",
    sgeunit:assert_equal(Res, ResultStr).

test_simpleif() ->
    yaws_tei:register_template(simple_if, simple_if(), sgte),
    {ok, F} = yaws_tei:lookup(simple_if),
    DThen = [{test, true}],
    DElse = [{test, false}],
    RThen = F(DThen),
    RElse = F(DElse),
    [sgeunit:assert_equal(RThen, "Start then branch"),
     sgeunit:assert_equal(RElse, "Start else branch")].

test_if() ->
    yaws_tei:register_template(if_str, if_string(), sgte),
    {ok, R} = yaws_tei:lookup(if_str),
    NameL = mountainList(),
    Data1 = [{testNames, true},
	    {nameList, NameL}],
    Data2 = [{testNames, false},
	    {noName, fun no_name/1}],
    Res1 = R(Data1),
    Res2 = R(Data2),
    [sgeunit:assert_equal(Res1, "Hello! Some Mountains: Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio Bye Bye."),
     sgeunit:assert_equal(Res2, "Hello! No Name Found Bye Bye.")].    
    
test_fif() ->
    yaws_tei:register_template(if_str, if_string(), sgte),
    {ok, R} = yaws_tei:lookup(if_str),
    NameL = mountainList(),
    Data = [{testNames, check_names(NameL)},
	    {noName, fun no_name/1},
	    {nameList, NameL}],
    Res = R(Data),
    sgeunit:assert_equal(Res, "Hello! Some Mountains: Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio Bye Bye.").

test_fif2() ->
    yaws_tei:register_template(if_str, if_string(), sgte),
    {ok, R} = yaws_tei:lookup(if_str),
    D1 = dict:new(),
    D2 = dict:store('testNames', check_names([]), D1),
    D3 = dict:store('noName', fun no_name/1, D2),
    D4 = dict:store('nameList', mountainList(), D3),
    Res = R(D4),
    sgeunit:assert_equal(Res, "Hello! No Name Found Bye Bye.").

test_nested_fif() ->
    yaws_tei:register_template(nested_if, nested_if_string(), sgte),
    {ok, R} = yaws_tei:lookup(nested_if),
    NameL = mountainList(),
    D1 = dict:new(),
    D2 = dict:store('testNames', check_names(NameL), D1),
    D3 = dict:store('noName', fun no_name/1, D2),
    D4 = dict:store('nameList', NameL, D3),
    Res = R(D4),
    sgeunit:assert_equal(Res, "Some Mountains: Monte Bianco, Cerro Torre, Mt. Everest, Catinaccio").

test_map() ->
    yaws_tei:register_template(row, print_mountain(), sgte),
    yaws_tei:register_template(rowlist, print_mountains(), sgte),
    {ok, Row} = yaws_tei:lookup(row),
    {ok, RowList} = yaws_tei:lookup(rowlist),
    Data = [{nameList, mountains()}, {printMountain, Row}],
    Res = RowList(Data),
    Rendered = "<ul><li><b>Monte Bianco</b></li><li><b>Cerro Torre</b></li><li><b>Mt. Everest</b></li><li><b>Catinaccio</b></li></ul>",
    sgeunit:assert_equal(Res, Rendered).

test_map_on_empty_list() ->
    yaws_tei:register_template(row, print_mountain(), sgte),
    yaws_tei:register_template(rowlist, print_mountains(), sgte),
    {ok, PrintM} = yaws_tei:lookup(row),
    {ok, PrintMList} = yaws_tei:lookup(rowlist),
    Data = [{nameList, empty()}, {printMountain, PrintM}],
    Res = PrintMList(Data),
    Rendered = "<ul></ul>",
    sgeunit:assert_equal(Res, Rendered).

test_mmap() ->
    yaws_tei:register_template(row1, row1(), sgte),
    yaws_tei:register_template(row2, row2(), sgte),
    yaws_tei:register_template(rowlist, print_mmap(), sgte),
    {ok, PrintMList} = yaws_tei:lookup(rowlist),
    {ok, R1} = yaws_tei:lookup(row1),
    {ok, R2} = yaws_tei:lookup(row2),
    Data = [{nameList, mountains()}, 
	    {row1, R1}, 
	    {row2, R2}],
    Res = PrintMList(Data),
    Rendered = "<ul>"++
	"<li class=\"riga1\"><b>Monte Bianco</b></li>"++
	"<li class=\"riga2\"><b>Cerro Torre</b></li>"++
	"<li class=\"riga1\"><b>Mt. Everest</b></li>"++
	"<li class=\"riga2\"><b>Catinaccio</b></li>"++
	"</ul>",
    sgeunit:assert_equal(Res, Rendered).

test_imap() ->
    yaws_tei:register_template(rowlist, print_inline_mountains(), sgte),
    {ok, PrintMList} = yaws_tei:lookup(rowlist),
    Data = [{nameList, mountains()}, {myClass, "listItem"}],
    Res = PrintMList(Data),
    Rendered = "<ul>"++
	"<li class=\"listItem\"><b>Monte Bianco</b></li>"++
	"<li class=\"listItem\"><b>Cerro Torre</b></li>"++
	"<li class=\"listItem\"><b>Mt. Everest</b></li>"++
	"<li class=\"listItem\"><b>Catinaccio</b></li>"++
	"</ul>",
    sgeunit:assert_equal(Res, Rendered).

test_imap_name_place() ->
    yaws_tei:register_template(rowlist, print_inline_mountain_place(), sgte),
    {ok, PrintMList} = yaws_tei:lookup(rowlist),
    Data = [{nameList, mountains2()}],
    Res = PrintMList(Data),
    Rendered = "<ul>"++
	"<li><b>Monte Bianco</b> - Alps</li>"++
	"<li><b>Cerro Torre</b> - Patagonia</li>"++
	"<li><b>Mt. Everest</b> - Himalaya</li>"++
	"<li><b>Catinaccio</b> - Dolomites</li>"++
	"</ul>",
    sgeunit:assert_equal(Res, Rendered).

test_imap2() ->
    yaws_tei:register_template(rowlist, print_inline_mountains2(), sgte),
    {ok, PrintMList} = yaws_tei:lookup(rowlist),
    Data = [{nameList, mountains()}, {myClass, "listItem"},  {myClass2, "listItem2"}],
    Res = PrintMList(Data),
    Rendered = "<ul>\n"++
	"<li class=\"listItem\"><b>Monte Bianco</b></li>\n"++
	"<li class=\"listItem2\"><b>Cerro Torre</b></li>\n"++
	"<li class=\"listItem\"><b>Mt. Everest</b></li>\n"++
	"<li class=\"listItem2\"><b>Catinaccio</b></li>\n"++
	"</ul>",
    sgeunit:assert_equal(Res, Rendered).

% test callable attribute
test_fun() ->
    MyF = fun(Data) ->
		  {ok, V} = dict:find(foo, Data),
		  "TEST: " ++ V
	  end,
    yaws_tei:register_template(tmpl_fun, tmpl_fun(), sgte),
    {ok, F} = yaws_tei:lookup(tmpl_fun),
    Res = F([{foo, "foooo"}, {callme, MyF}]),
    sgeunit:assert_equal(Res, "aaaa TEST: foooo bbb").

%test on a non existent file
test_file() ->
    yaws_tei:register_file("myfile.tmpl", sgte),
    {ok, _F} = yaws_tei:lookup(tmpl_fun),
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
	"$map row1 row2 nameList$" ++
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
