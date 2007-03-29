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
%%% > {ok, Compiled} = sgte:compile(TmplStr),
%%% > sgte:render(Compiled, Data).
%%% or:
%%% > {ok, Compiled} = sgte:compile_file(FileName),
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

%%yaws_tei is not in a public release yet -behaviour(yaws_tei).

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
compile(T) when is_binary(T) ->
    sgte_parse:parse(binary_to_list(T));
compile(T) when is_list(T) ->
    sgte_parse:parse(T);
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
	{ok, Bin} ->
	    compile(Bin);
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
	{ok, Bin} ->
	    compile(Bin);
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
render(Compiled, Data) when is_function(Compiled) ->
    render_final(Compiled, Data);
render(Compiled, Data) ->
    lists:flatten([render_element(X, Data) || X <- Compiled]).
%% used for map Attr is a tuple {attr, Value}
render(Compiled, Data, Attr) when is_list(Attr) ->
    Data1 = dict:merge(fun(_K, _V1, V2) -> V2 end, Data, dict:from_list(Attr)),
    lists:flatten([render_element(X, Data1) || X <- Compiled]);
render(Compiled, Data, Attr) when is_function(Compiled) ->
    {K, V} = Attr,
    Data1 = dict:store(K, V, Data),
    render_final(Compiled, Data1);
render(Compiled, Data, Attr) ->
    {K, V} = Attr,
    Data1 = dict:store(K, V, Data),
    lists:flatten([render_element(X, Data1) || X <- Compiled]).

    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

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
