-module(eunit_render_map).

-include_lib("eunit/include/eunit.hrl").

%%--------------------
%%
%% Tests
%%
%%--------------------
%%
%% Render Test
%%
map_test_() ->
    {ok, PrintM} = sgte:compile(print_mountain()),
    {ok, PrintMList} = sgte:compile(print_mountains()),
    Data = [{nameList, mountains()}, {printMountain, PrintM}],
    Res = sgte:render_str(PrintMList, Data),
    Rendered = "<ul><li><b>Monte Bianco</b></li><li><b>Cerro Torre</b></li><li><b>Mt. Everest</b></li><li><b>Catinaccio</b></li></ul>",
    Data2 = [{nameList, empty()}, {printMountain, PrintM}],
    Res2 = sgte:render_str(PrintMList, Data2),
    Rendered2 = "<ul></ul>",
    [?_assert(Res =:= Rendered), 
     ?_assert(Res2 =:= Rendered2)].

mapl_test_() ->
    {ok, RowTmpl} = sgte:compile("- $attr$\n"),
    {ok, MapLTmpl} = sgte:compile("$mapl rowTmpl nameList$"),
    Data = [{rowTmpl, RowTmpl}, {nameList, mountain_list()}],
    Res = sgte:render_str(MapLTmpl, Data),
    Rendered = "- Monte Bianco\n"
	"- Cerro Torre\n"
	"- Mt. Everest\n"
	"- Catinaccio\n",
    ?_assert(Res =:= Rendered).

mapj_test_() ->
    {ok, RowTmpl} = sgte:compile("- $el$"),
    {ok, Separator} = sgte:compile(", \n"),
    {ok, MapJ} = sgte:compile("$mapj row values sep$"),
    Data = [{row, RowTmpl}, {sep, Separator}, 
	    {values, [{el, "foo"}, {el, "bar"}, {el, "baz"}]}],
    Res = sgte:render_str(MapJ, Data),
    Rendered = "- foo, \n- bar, \n- baz",
    ?_assert(Res =:= Rendered).

mmap_test_() ->
    {ok, PrintMList} = sgte:compile(print_mmap()),
    {ok, R1} = sgte:compile(row1()),
    {ok, R2} = sgte:compile(row2()),
    Data = [{nameList, mountains()}, 
	    {row1, R1}, 
	    {row2, R2}],
    Res = sgte:render_str(PrintMList, Data),
    Rendered = "<ul>"++
	"<li class=\"riga1\"><b>Monte Bianco</b></li>"++
	"<li class=\"riga2\"><b>Cerro Torre</b></li>"++
	"<li class=\"riga1\"><b>Mt. Everest</b></li>"++
	"<li class=\"riga2\"><b>Catinaccio</b></li>"++
	"</ul>",
    ?_assert(Res =:= Rendered).

imap_test_() ->
    {ok, PrintMList} = sgte:compile(print_inline_mountains()),
    Data = [{nameList, mountains()}, {myClass, "listItem"}],
    Res = sgte:render_str(PrintMList, Data),
    Rendered = "<ul>"++
	"<li class=\"listItem\"><b>Monte Bianco</b></li>"++
	"<li class=\"listItem\"><b>Cerro Torre</b></li>"++
	"<li class=\"listItem\"><b>Mt. Everest</b></li>"++
	"<li class=\"listItem\"><b>Catinaccio</b></li>"++
	"</ul>",
    {ok, PrintMList2} = sgte:compile(print_inline_mountain_place()),
    Data2 = [{nameList, mountains2()}],
    Res2 = sgte:render_str(PrintMList2, Data2),
    Rendered2 = "<ul>"++
	"<li><b>Monte Bianco</b> - Alps</li>"++
	"<li><b>Cerro Torre</b> - Patagonia</li>"++
	"<li><b>Mt. Everest</b> - Himalaya</li>"++
	"<li><b>Catinaccio</b> - Dolomites</li>"++
	"</ul>",
    {ok, PrintMList3} = sgte:compile(print_inline_mountains2()),
    Data3 = [{nameList, mountains()}, {myClass, "listItem"},  {myClass2, "listItem2"}],
    Res3 = sgte:render_str(PrintMList3, Data3),
    Rendered3 = "<ul>\n"++
	"<li class=\"listItem\"><b>Monte Bianco</b></li>\n"++
	"<li class=\"listItem\"><b>Cerro Torre</b></li>\n"++
	"<li class=\"listItem\"><b>Mt. Everest</b></li>\n"++
	"<li class=\"listItem\"><b>Catinaccio</b></li>\n"++
	"</ul>",
    %% test bug in js code 
    {ok, C} = sgte:compile(imap_js()),
    RenderedJs = sgte:render_str(C, [{owners, 
                                      [{owner, "tobbe"}, 
                                       {owner, "magnus"}
                                      ]}]
                                ),
    ResultJs = "\"#tobbe\": function(t) {save_owner(\"tobbe\",t.id);}, "++
	"\"#magnus\": function(t) {save_owner(\"magnus\",t.id);}, ",
    %% test comma bug
    {ok, Comma} = sgte:compile(imap_comma()),
    RendComma = sgte:render_str(Comma, [{attrList, 
                                         [{attr, "First Attribute"}, 
                                          {attr, "and the Second"}
                                         ]}]
                               ),
    ResComma = "First Attribute, and the Second, ",
    [?_assert(Res =:= Rendered), 
     ?_assert(Res2 =:= Rendered2),
     ?_assert(Res3 =:= Rendered3),
     ?_assert(ResultJs =:= RenderedJs),
     ?_assert(ResComma =:= RendComma)].


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
