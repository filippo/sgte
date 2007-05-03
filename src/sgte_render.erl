%%%-------------------------------------------------------------------
%%% File    : sgte_render.erl
%%% Author  : filippo pacini <pacini@sgconsulting.it>
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
%%% 2007 S.G. Consulting srl. All Rights Reserved.
%%%
%%% Description : 
%%%
%%% Created : 16 Apr 2007 by filippo pacini <pacini@sgconsulting.it>
%%%-------------------------------------------------------------------
-module(sgte_render).

-export([render/2]).

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
    render(Compiled, dict:from_list(Data));
render(Compiled, Data) when is_function(Compiled) ->
    render_final(Compiled, Data);
render(Compiled, Data) ->
    lists:flatten([render_element(X, Data) || X <- Compiled]).

%% used for map Attr is a tuple {Key, Value}
render(Compiled, Data, Attr) when is_list(Attr) ->
    Data1 = dict:merge(fun(_K, _V1, V2) -> V2 end, Data, dict:from_list(Attr)),
    lists:flatten([render_element(X, Data1) || X <- Compiled]);
render(Compiled, Data, Attr) when is_function(Compiled) ->
    {K, V} = Attr,
    render_final(Compiled, dict:store(K, V, Data));
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
render_final(Term) when is_pid(Term) ->
    render_error({error, {Term, pid, invalid_data}});
render_final(Term) when is_tuple(Term) ->
    render_error({error, {Term, tuple, invalid_data}});
render_final(Term) ->
    Term.
    
%%--------------------------------------------------------------------
%% Function: render_element(Term, Data) -> Value
%% Description: render an element in the parsed template.
%%--------------------------------------------------------------------
render_element({attribute, Term, Line}, Data) ->
    case get_value(Term, Data, attribute) of
	{error, X} ->
	    render_error({error, X, {line, Line}});
	Value ->
	    render_final(Value, Data)
    end;
render_element({join, {Separator, Term}, Line}, Data) ->
    case get_value(Term, Data, join) of
	{error, X} ->
	    render_error({error, X, {line, Line}});
	ValueList ->
	    Concat = lists:flatten([X++Separator || X <- ValueList]),
	    Value = string:sub_string(Concat, 1, length(Concat)-length(Separator)),
	    Value
    end;

render_element({include, Tmpl, Line}, Data) -> %% include template passing all data
    case get_value(Tmpl, Data, include) of
	{error, X} ->
	    render_error({error, X, {line, Line}});
	Compiled ->
	    render(Compiled, Data)
    end;

render_element({apply, {Callable, Var}, Line}, Data) -> %% apply first element to Var
    case get_value(Callable, Data, apply) of
	{error, X} ->
	    render_error({error, X, {line, Line}});
	ToCall ->
	    case get_value(Var, Data, apply) of
		{error, X} ->
		    render_error({error, X, {line, Line}});
		Value ->
		    render_final(ToCall, Value)
	    end
    end;

render_element({map, {Tmpl, Term}, Line}, Data) ->
    case {get_value(Tmpl, Data, map), get_value(Term, Data, map)} of
	{{error, X}, _} ->
	    render_error({error, X, {line, Line}});
	{_, {error, X}} ->
	    render_error({error, X, {line, Line}});
	{CT, ValueList} ->
	    [render(CT, Data, V) || V <- ValueList]
    end;
render_element({mapl, {Tmpl, Term}, Line}, Data) ->
    case {get_value(Tmpl, Data, mapl), get_value(Term, Data, mapl)} of
	{{error, X}, _} ->
	    render_error({error, X, {line, Line}});
	{_, {error, X}} ->
	    render_error({error, X, {line, Line}});
	{CT, ValueList} ->
	    [render(CT, Data, {attr, V}) || V <- ValueList]
    end;
render_element({mmap, {Tmpl, Term}, Line}, Data) ->
    case get_value(Term, Data, mmap) of
	{error, X} ->
	    render_error({error, X, {line, Line}});
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
render_element({imap, {[TmplList], Term}, Line}, Data) ->
    case get_value(Term, Data, imap) of
	{error, X} ->
	    render_error({error, X, {line, Line}});
	ValueList ->
	    [render(TmplList, Data, V) || V <- ValueList]
    end;
render_element({gettext, Key, Line}, Data) ->
    case get_value(gettext_lc, Data, gettext) of
	{error, X} ->
	    render_error({error, X, {line, Line}});
	LC ->
	    case catch gettext:key2str(Key, LC) of
		Translation when list(Translation) -> Translation;
		_ -> Key
	    end 
    end;
render_element({ift, {{attribute, Test}, Then, Else}, Line}, Data) ->
    case get_value(Test, Data, ift) of
	{error, X} ->
	    render_error({error, X, {line, Line}});
	TestP ->
	    case render_final(TestP, Data) of
		true ->
		    render(Then, Data);
		_ ->
		    render(Else, Data)
	    end
    end;
render_element({ift, {{attribute, Test}, Then}, Line}, Data) ->
    case get_value(Test, Data, ift) of
	{error, X} ->
	    render_error({error, X, {line, Line}});
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

render_error({error, {TmplEl, Key, not_found}, {line, LineNo}}) ->
    io_lib:format("[SGTE Error: template: ~p - key ~p not found on line ~p]", [TmplEl, Key, LineNo]);
%% Render an error in the data type format
render_error({error, {Term, DataType, invalid_data}}) ->
    io_lib:format("[SGTE Error: invalid data type: ~p is a ~p. String expected]", [Term, DataType]);
%% Render an ErrMsg passed as a string
render_error({error, {TmplEl, ErrMsg}, {line, LineNo}}) when is_list(ErrMsg) ->
    io_lib:format("[SGTE Error: ~p ~s on line ~p]", [TmplEl, ErrMsg, LineNo]).

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
