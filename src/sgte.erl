%%%-------------------------------------------------------------------
%%% File    : sgte.erl
%%% Author  : filippo pacini <pacini@sgconsulting.it>
%%%
%%% License :
%%% The contents of this file are subject to the Mozilla Public
%%% License Version 1.1 (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of
%%% the License at http://www.mozilla.org/MPL/
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and
%%% limitations under the License.
%%% The Initial Developer of the Original Code is S.G. Consulting
%%% srl. Portions created by S.G. Consulting s.r.l. are Copyright (C)
%%% 2006 S.G. Consulting srl. All Rights Reserved.
%%%
%%% Description :
%%% @doc <p>The <em>SGTE</em> module is a library implementing a Template Engine
%%% The template system is inspired on <a href="http://www.stringtemplate.org" target="_blank">String Template</a>
%%% </p><p>
%%% The use of the Engige is as simple as (from the command line):
%%% <pre>
%%% > sgte:start_link(),
%%% > {ok, Compiled} = sgte:template(Template),
%%% > sgte:render(Compiled, Data).
%%% </pre>
%%% Where Template can be either a string or a tuple {file, FileName}
%%% Data can be a Dict or a list of tuple (e.g. [{attr1, Val1}, {attr2, Val2}])
%%% Values can be a simple value or a function/1. In this case the function is 
%%% called with Data as an argument.
%%% </p><p>
%%% Template Features:
%%% <ol>
%%% <li>attribute reference: e.g. <pre>$name$</pre></li>
%%% <li>template reference: e.g. <pre>$include tmpl$</pre>.</li>
%%% <li>application of an attribute to another: e.g. <pre>$apply myFun
%%% aVar$</pre>. When the first attribute is callable you get the
%%% result of myFun(aVar). Otherwhise the result is the value of
%%% myFun.</li>
%%% <li>conditional evaluation: e.g. 
%%%   <pre>
%%%   $if title$
%%%       &lt;h1&gt;$title$&lt;/h1&gt;
%%%   $else
%%%       &lt;h1&gt;default title&lt;/h1&gt;
%%%   $end if$
%%%   </pre></li>
%%% <li>template application to a list of elements: e.g. if names is a list [{username, name1}, {username, name2}]
%%%   <pre>$map li names$</pre>
%%%   map li template to names. Each element in names is passed to the template with name attr.
%%%   If li is the template: <pre>&lt;li&gt;&lt;b&gt;$username$&lt;/b&gt;&lt;/li&gt;</pre>
%%%   We get the result:
%%%          <pre>
%%%          &lt;li&gt;&lt;b&gt;name1&lt;/b&gt;&lt;/li&gt;
%%%          &lt;li&gt;&lt;b&gt;name2&lt;/b&gt;&lt;/li&gt;
%%%          </pre>
%%%   Another ways to express the same template inline is:
%%%   <pre>$map:{&lt;li&gt;&lt;b&gt;$username$&lt;/b&gt;&lt;/li&gt;} names$</pre>
%%% </li>
%%% <li>alternate several templates on a list of elements:
%%%   <pre>$map row1 row2 row3 names$</pre>
%%%   Inline syntax:
%%%   <pre>
%%%   $map:{&lt;li class="row1"&gt;$username$&lt;/li&gt;, 
%%%         &lt;li class="row2"&gt;$username$&lt;/li&gt;, 
%%%         &lt;li class="row3"&gt;$username$&lt;/li&gt;} names$
%%%   </pre></li>
%%% <li>join of items using a separator:
%%%   <pre>SELECT $join "," columns$ FROM $table$;</pre>
%%% </li>
%%% </ol></p>
%%% @end
%%% Created : 13 Sep 2006 by filippo pacini <pacini@sgconsulting.it>
%%%-------------------------------------------------------------------
-module(sgte).

-author("$Author$").
-version("$Rev$").
-date("$Date$").

%% API
-export([compile/1, compile_file/1, render/2]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Compiles the template string T and returns the compiled 
%% template or an error.
%% <pre>
%% Expects:
%%  T - The string containing the template.
%%
%% Types:
%%  T = [char()]
%%  Compiled = [char()|tuple()]
%%  Reason = tuple()
%% </pre>
%% @spec compile(T) -> {ok, Compiled} | {error,Reason}
%% @end
%%--------------------------------------------------------------------
compile(T) when is_list(T) ->
    parse(T);
%%--------------------------------------------------------------------
%% @doc Compiles the template file FileName and returns the compiled 
%% template or an error.
%% <pre>
%% Expects:
%%  {file, FileName} - The file containing the template.
%%
%% Types:
%%  FileName = [char()]
%%  Compiled = [char()|tuple()]
%%  Reason = tuple()
%% </pre>
%% @spec compile({file, FileName}) -> {ok, Compiled} | {error,Reason}
%% @end
%%--------------------------------------------------------------------
compile({file, FileName}) ->
    case file:read_file(FileName) of
	{ok, Binary} ->
	    Data = binary_to_list(Binary),
	    compile(Data);
	Err -> 
	    Err
    end.
%%--------------------------------------------------------------------
%% @doc Compiles the template file FileName and returns the compiled 
%% template or an error.
%% <pre>
%% Expects:
%%  FileName - The file containing the template.
%%
%% Types:
%%  FileName = [char()]
%%  Compiled = [char()|tuple()]
%%  Reason = tuple()
%% </pre>
%% @spec compile_file(FileName) -> {ok, Compiled} | {error,Reason}
%% @end
%%--------------------------------------------------------------------
compile_file(FileName) ->
    case file:read_file(FileName) of
	{ok, Binary} ->
	    Data = binary_to_list(Binary),
	    compile(Data);
	Err -> 
	    Err
    end.

%%--------------------------------------------------------------------
%% @doc Renders the compiled template and returns it
%% <pre>
%% Expects:
%%  Compiled - The compiled template.
%%  Data - The data referred in the template
%%
%% Types:
%%  Compiled = [char()|tuple()]
%%  Data = [tuple()]|dict()
%%  Rendered = [char()]
%% </pre>
%% @spec render(Compiled, Data) -> Rendered
%% @end
%%--------------------------------------------------------------------
render(Compiled, Data) when is_list(Data) ->
    Data1 = list_to_dict(Data), %% convert data from list to dict
    render(Compiled, Data1);
render(Compiled, Data) ->
    lists:flatten([render_element(X, Data) || X <- Compiled]).
%% used for map Attr is a tuple {attr, Value}
render(Compiled, Data, Attr) when is_list(Attr) ->
    Data1 = dict:merge(fun(_K, _V1, V2) -> V2 end, Data, dict:from_list(Attr)),
    lists:flatten([render_element(X, Data1) || X <- Compiled]);
render(Compiled, Data, Attr) ->
    {K, V} = Attr,
    Data1 = dict:store(K, V, Data),
    lists:flatten([render_element(X, Data1) || X <- Compiled]).

    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function: parse(Template) -> {ok, Parsed} | {error,Reason}
%% Description: Returns the parsed template.
%%--------------------------------------------------------------------
parse(Template) ->
    case parse(Template, []) of
	{error, X} ->
	    {error, X};
	Parsed ->
	    {ok, lists:reverse(Parsed)}
    end.
parse([], Parsed) ->
    Parsed;
parse("$if "++Rest, Parsed) ->
    P = collect_ift(Rest, [], {}),
    case P of
	{error, X} ->
	    {error, X};
	{ift, IfToken, Rest1} ->
	    case parse_token({ift, IfToken}) of
		{error, E} -> {error, E};
		ParsedIf -> 
		    parse(Rest1, [ParsedIf|Parsed])
	    end
    end;
parse([H|Rest], Parsed) when [H] == "$" ->   %% start token: call collect_token to get the complete token.
    case collect_token(Rest, []) of
	{error, X} ->
	    {error, X};
	{token, Token, Rest1} ->
	    case parse_token(Token) of
		{error, E} -> {error, E};
		T ->
		    parse(Rest1, [T|Parsed])
	    end;
	{string, Token, Rest1} ->  %% escaped $ character
	    parse(Rest1, [Token|Parsed])
    end;
parse([H|Rest], Parsed) ->
    parse(Rest, [H|Parsed]).

%%--------------------------------------------------------------------
%% Function: collect_token(TemplatePart, TokenSoFar) -> 
%%                                       {Token, RemainingTemplate} |
%%                                       {error,Reason} 
%% Description: If called I'm in a token. collect_token consumes the
%% Template until the end of the token is found.  Returns the Token
%% found converted in an atom or a tuple.  If the end of the Template
%% is and no end token is found returns {error, end_token_not_found}.
%%--------------------------------------------------------------------
collect_token(Template, Token) ->
    collect_token(Template, Token, 0).

collect_token([], Token, _Inline) ->
    {error, {end_token_not_found, lists:reverse(Token)}};
collect_token("\\$"++Rest, Token, Inline) -> %% Escape sequence for \, $, {, }
    collect_token(Rest, ["$"|Token], Inline);
collect_token("\\{"++Rest, Token, Inline) -> %% Escape sequence for \, $, {, }
    collect_token(Rest, ["{"|Token], Inline);
collect_token("\\}"++Rest, Token, Inline) -> %% Escape sequence for \, $, {, }
    collect_token(Rest, ["}"|Token], Inline);
collect_token("\\\\"++Rest, Token, Inline) -> %% Escape sequence for \, $, {, }
    collect_token(Rest, ["\\"|Token], Inline);
collect_token([H|Rest], Token, 0) when [H] == "$" -> %% Inline == 0 so I'm at the end of the parse
    case Token of
	[] -> {string, H, Rest}; %% $$ found in the template (It's the escape sequence for $ sign
	_ ->  {token, lists:reverse(Token), Rest}
    end;
collect_token([H|Rest], Token, Inline) when [H] == "$" -> %% Inline > 0 so I'm in a inline template
    {_X, InnerToken, Rest1} = collect_token(Rest, []),
    Inner1 = parse_token(InnerToken),
    collect_token(Rest1, [Inner1|Token], Inline);
collect_token([H|Rest], Token, Inline) when [H] == "{" ->
    Inline1 = Inline + 1, 
    collect_token(Rest, [H|Token], Inline1);
collect_token([H|Rest], Token, Inline) when [H] == "}" ->
    Inline1 = Inline - 1, 
    collect_token(Rest, [H|Token], Inline1);
collect_token([H|Rest], Token, Inline) ->
    collect_token(Rest, [H|Token], Inline).

collect_ift([], Token, T) ->
    {error, {end_token_not_found, ift, T, lists:reverse(Token)}};

collect_ift("\\$"++Rest, Token, T) -> %% Escape sequence for \, $, {, }
    collect_ift(Rest, ["$"|Token], T);

collect_ift("\\\\"++Rest, Token, T) -> %% Escape sequence for \, $, {, }
    collect_ift(Rest, ["\\"|Token], T);

collect_ift("$end if$"++Rest, Token, {Test, Then}) ->
    {ift, {Test, Then, lists:reverse(Token)}, Rest};

collect_ift("$end if$"++Rest, Token, {Test}) ->
    {ift, {Test, lists:reverse(Token)}, Rest};

collect_ift("$else$"++Rest, Token, {Test}) ->
    collect_ift(Rest, [], {Test, lists:reverse(Token)});

%% Nested if
collect_ift("$if "++Rest, Token, {Test}) ->
    case collect_ift(Rest, [], {}) of
	{ift, InnerIf, Rest1} ->
	    case parse_token({ift, InnerIf}) of
		{error, E} -> 
		    collect_ift(Rest1, [{error, E}, lists:reverse(Token)], {Test});    
		ParsedIf -> 
		    collect_ift(Rest1,[ParsedIf, lists:reverse(Token)], {Test})
	    end;
	{error, E} -> 
	    collect_ift(Rest, [{error, E}, lists:reverse(Token)], {Test})
    end;


collect_ift([H|Rest], Token, {}) when [H] == "$" ->
    collect_ift(Rest, [], {lists:reverse(Token)});

collect_ift([H|Rest], Token, T) ->
    collect_ift(Rest, [H|Token], T).

%% Template Rules
%% if token
parse_token({ift, {Test, Then, Else}}) ->
    case {parse(Then), parse(Else)} of
	{{error, X}, _} -> {error, X};
	{_, {error, Y}} -> {error, Y};
	{{ok, CThen}, {ok, CElse}} -> 
	    {ift, 
	     {{attribute, list_to_atom(string:strip(Test))}, 
	      CThen, CElse}
	    }
    end;
parse_token({ift, {Test, Then}}) ->
    case parse(Then) of
	{error, X} -> {error, X};
	{ok, CThen} ->
	    {ift, {{attribute, list_to_atom(string:strip(Test))}, CThen}}
    end;

%% call template
parse_token("include "++Token) ->
    {include, erlang:list_to_atom(Token)};

%% apply first argument which must be callable to second argument
parse_token("apply "++Token) ->
    Split = string:tokens(Token, " "),
    case length(Split) of
	2 -> 
	    {apply, erlang:list_to_atom(lists:nth(1, Split)), erlang:list_to_atom(lists:nth(2, Split))};
	_ ->
	    {error, {invalid_apply, Token}}
    end;

%% map template
parse_token("map "++Token) ->
    Split = string:tokens(Token, " "),
    case length(Split) of
	1 -> {error, {invalid_map, Token}};
	0 -> {error, {invalid_map, Token}};
	X ->
	    VarList = lists:nth(X, Split),
	    TmplList = lists:sublist(Split, X-1),
	    {map, [erlang:list_to_atom(El) || El <- TmplList], erlang:list_to_atom(VarList)}
    end;

%% inline map template
parse_token("map:{"++Token) ->
    case parse_inline(Token, []) of
	{error, X} ->
	    {error, X};
	{Inline, VarList} when VarList == ""-> 
	    {error, {invalid_map, Inline}};
	{Inline, VarList} -> 
	    Split = string:tokens(Inline, ","),
	    Split1 = cons_when(Split, fun(El) -> lists:last(El) == $\\ end, []),
	    {imap, Split1, erlang:list_to_atom(VarList)};
	_ -> 
	    {error, {invalid_map, Token}}
    end;

%% join template
parse_token("join \""++Token) ->
    Split = string:tokens(Token, "\""),
    case length(Split) of
	2 ->
	    Attr = lists:last(Split),
	    SepList = lists:sublist(Split, length(Split)-1),
	    Concat = lists:flatten([X++"\"" || X <- SepList]),
	    Separator = lists:sublist(Concat, length(Concat)-1),
	    {join, Separator, erlang:list_to_atom(string:strip(Attr))};
	_ -> {error, {invalid_join, Token}}
    end;

%% simple attribute
parse_token(Token) ->
    {attribute, erlang:list_to_atom(Token)}.

%% parse inline template
parse_inline("} "++Rest, SoFar) ->
    {lists:reverse(SoFar), string:strip(Rest)};
parse_inline("}", SoFar) ->
    {error, {invalid_map, lists:reverse(SoFar)}};
parse_inline([H|T], SoFar) ->
    parse_inline(T, [H|SoFar]).

%% redder_final(Attribute, Data) -> Attribute|Attribute(Data)
%% render on final attribute.  If it's a function call it on Data,
%% else return attribute
render_final(Attribute, Data) when is_function(Attribute) ->
    render_final(Attribute(Data));
render_final(Attribute, _Data) ->
    render_final(Attribute).

render_final(Term) when is_boolean(Term) ->
    Term;
render_final(Term) when is_atom(Term) ->
    render_error({error, {Term, atom, invalid_data}});
render_final(Term) when is_function(Term) ->
    render_error({error, {Term, function, invalid_data}});
render_final(Term) when is_pid(Term) ->
    render_error({error, {Term, pid, invalid_data}});
render_final(Term) ->
    Term.
    
%%--------------------------------------------------------------------
%% Function: render_element(Term, Data) -> Value
%% Description: render an element in the parsed template.
%%--------------------------------------------------------------------
render_element({attribute, Term}, Data) ->
    case get_value(Term, Data, attribute) of
	{error, X} ->
	    render_error({error, X});
	Value ->
	    render_final(Value, Data)
    end;
render_element({join, Separator, Term}, Data) ->
    case get_value(Term, Data, join) of
	{error, X} ->
	    render_error({error, X});
	ValueList ->
	    Concat = lists:flatten([X++Separator || X <- ValueList]),
	    Value = string:sub_string(Concat, 1, length(Concat)-length(Separator)),
	    Value
    end;
render_element({include, Tmpl}, Data) -> %% include template passing all data
    case get_value(Tmpl, Data, include) of
	{error, X} ->
	    render_error({error, X});
	Compiled ->
	    render(Compiled, Data)
    end;

render_element({apply, Callable, Var}, Data) -> %% apply first element to Var
    case get_value(Callable, Data, apply) of
	{error, X} ->
	    render_error({error, X});
	ToCall ->
	    case get_value(Var, Data, apply) of
		{error, X} ->
		    render_error({error, X});
		Value ->
		    render_final(ToCall, Value)
	    end
    end;

render_element({map, Tmpl, Term}, Data) ->
    case get_value(Term, Data, map) of
	{error, X} ->
	    render_error({error, X});
	ValueList ->
	    ExtractTmpl = fun(El, Acc) ->
				  Compiled = get_value(El, Data, map),
				  [Compiled|Acc]
			  end,
	    CompiledList = lists:reverse(lists:foldl(ExtractTmpl, [], Tmpl)),
	    % Zipped is a tuple list: [{tmpl1, val1}, {tmpl2, val2},{tmpl1, val3} ...]
	    Zipped = group(CompiledList, ValueList), 
	    [render(CT, Data, V) || {CT, V} <- Zipped]
    end;
render_element({imap, TmplList, Term}, Data) ->
    case get_value(Term, Data, imap) of
	{error, X} ->
	    render_error({error, X});
	ValueList ->
	    % Zipped is a tuple list: [{tmpl1, val1}, {tmpl2, val2},{tmpl1, val3} ...]
	    Zipped = group(TmplList, ValueList), 
	    [render(CT, Data, V) || {CT, V} <- Zipped]
    end;
render_element({ift, {{attribute, Test}, Then, Else}}, Data) ->
    case get_value(Test, Data, ift) of
	{error, X} ->
	    render_error({error, X});
	TestP ->
	    case render_final(TestP, Data) of
		true ->
		    Res = render(Then, Data);
		_ ->
		    Res = render(Else, Data)
	    end,
	    Res
    end;
render_element({ift, {{attribute, Test}, Then}}, Data) ->
    case get_value(Test, Data, ift) of
	{error, X} ->
	    render_error({error, X});
	TestP ->
	    case render_final(TestP, Data) of
		true ->
		    Res = render(Then, Data);
		_ ->
		    Res = []
	    end,
	    Res
    end;
render_element(Term, _Data) ->
    render_final(Term).

%% Render an error in the get_value
render_error({error, {TmplEl, Key, not_found}}) ->
    io_lib:format("[SGTE Error: template: ~p - key ~p not found]", [TmplEl, Key]);
%% Render an error in the data type format
render_error({error, {Term, DataType, invalid_data}}) ->
    io_lib:format("[SGTE Error: invalid data type: ~p is a ~p. String expected]", [Term, DataType]).

%%--------------------------------------------------------------------
%% Utilities
%%--------------------------------------------------------------------
%% get Key from Dict. If Key is not found returns an error.
%% Where is used to return a more desciptive error.
get_value(Key, Dict, Where) ->
    case dict:find(Key, Dict) of
	{ok, V} ->
	    V;
	error ->
	    {error, {Where, Key, not_found}}
    end.

%% Test predicate P on the head of the first argument. On success cons
%% the first argument and second argument.  On failure append the head
%% to the result and call itself recursively.
cons_when([], _P, Res) ->
    lists:reverse(Res);
cons_when([H|Rest], P, Res) ->
    case P(H) of
	true ->
	    case length(Rest) > 0 of
		true ->
		    [HR|Tail] = Rest,
		    H1 = lists:sublist(H, length(H)-1) ++ "," ++ HR,
		    cons_when(Tail, P, [H1|Res]);
		_ ->
		    H1 = lists:sublist(H, length(H)-1) ++ ",",
		    cons_when(Rest, P, [H1|Res])
	    end;
	_ ->
	    cons_when(Rest, P, [H|Res])
    end.

%%--------------------------------------------------------------------
%% Function: list_to_dict(List) -> Dict
%% Description: List is a list of {Key, Value} tuples. 
%% Build a dict from the list and returns it.
%%--------------------------------------------------------------------
list_to_dict(List) ->
    Converter = fun(El, Acc) ->
			{K, V} = El,
			dict:store(K, V, Acc)
		end,
    lists:foldl(Converter, dict:new(), List).

%% build a list of tuple [{El1, El2}].
%% If length(L1) < length(L2) when elements of L1 end restart with the complete list L1
%% If length(L1) > length(L2) elements in L1 whit index > length(L2) are ignored
%% e.g.
%% L1 = [1, 2, 3], L2 = [a, b] -> [{1, a}, {2, b}]
%% L1 = [1, 2], L2 = [a, b, c, d, e] -> [{1, a}, {2, b}, {1, c}, {2, d}, {1, e}]
group(L1, L2) ->
    group(L1, L2, L1, []).
group(_, [], _Recurring, Result) ->
    lists:flatten(lists:reverse(Result));
group([], [H2|R2], Recurring, Result) ->
    group(Recurring, [H2|R2], Recurring, Result);
group([H1|R1], [H2|R2], Recurring, Result) ->
    group(R1, R2, Recurring, [{H1, H2}|Result]).
