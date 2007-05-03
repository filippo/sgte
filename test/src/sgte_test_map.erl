-module(sgte_test_map).

-export([test_map/0, test_mapl/0, test_map_on_empty_list/0, test_mmap/0, test_mapj/0]).
-export([test_imap/0,test_imap2/0, test_imap_name_place/0]).
-export([test_imap_js/0, test_imap_comma/0]).

%%--------------------
%%
%% Tests
%%
%%--------------------
%%
%% Render Test
%%
test_map() ->
    {ok, PrintM} = sgte:compile(print_mountain()),
    {ok, PrintMList} = sgte:compile(print_mountains()),
    Data = [{nameList, mountains()}, {printMountain, PrintM}],
    Res = sgte:render(PrintMList, Data),
    Rendered = "<ul><li><b>Monte Bianco</b></li><li><b>Cerro Torre</b></li><li><b>Mt. Everest</b></li><li><b>Catinaccio</b></li></ul>",
    sgeunit:assert_equal(Res, Rendered).

test_mapl() ->
    {ok, RowTmpl} = sgte:compile("- $attr$\n"),
    {ok, MapLTmpl} = sgte:compile("$mapl rowTmpl nameList$"),
    Data = [{rowTmpl, RowTmpl}, {nameList, mountain_list()}],
    Res = sgte:render(MapLTmpl, Data),
    Rendered = "- Monte Bianco\n"
	"- Cerro Torre\n"
	"- Mt. Everest\n"
	"- Catinaccio\n",
    sgeunit:assert_equal(Res, Rendered).

test_mapj() ->
    {ok, RowTmpl} = sgte:compile("- $el$"),
    {ok, Separator} = sgte:compile(", \n"),
    {ok, MapJ} = sgte:compile("$mapj row values sep$"),
    Data = [{row, RowTmpl}, {sep, Separator}, 
	    {values, [{el, "foo"}, {el, "bar"}, {el, "baz"}]}],
    Res = sgte:render(MapJ, Data),
    Rendered = "- foo, \n- bar, \n- baz",
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
	"<li class=\"listItem\"><b>Cerro Torre</b></li>\n"++
	"<li class=\"listItem\"><b>Mt. Everest</b></li>\n"++
	"<li class=\"listItem\"><b>Catinaccio</b></li>\n"++
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

%%--------------------
%%
%% Internal functions
%%
%%--------------------
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
	"$map:{<li class=\"$myClass$\"><b>$mountain$</b></li>\n"++
	"} nameList$" ++
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

imap_js() ->
    "$map:{\"#$owner$\": function(t) {save_owner(\"$owner$\",t.id);}, } owners$".

imap_comma() ->
    "$map:{$attr$, } attrList$".

empty() ->
    [].

mountains() ->
    [{mountain, "Monte Bianco"}, {mountain, "Cerro Torre"}, {mountain, "Mt. Everest"}, {mountain, "Catinaccio"}].

mountain_list() ->
    ["Monte Bianco", "Cerro Torre", "Mt. Everest", "Catinaccio"].

mountains2() ->
    [[{mountain, "Monte Bianco"}, {place, "Alps"}], 
     [{mountain, "Cerro Torre"}, {place, "Patagonia"}], 
     [{mountain, "Mt. Everest"}, {place, "Himalaya"}],
     [{mountain, "Catinaccio"}, {place, "Dolomites"}]].
