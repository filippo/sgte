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
%%% @doc 
%%% <p>Renders the compiled template and returns the 
%%% resulting string.</p>
%%%
%%% <p>This module is not meant to be used directly. It's called 
%%% through the interface of the sgte module.</p>
%%% @end
%%%
%%% Created : 16 Apr 2007 by filippo pacini <pacini@sgconsulting.it>
%%%-------------------------------------------------------------------
-module(sgte_render).

-ifdef(ERLHIVE).
-import(.lists).    % erlhive uses package notation
-import(.string).   % ditto
-endif.

-export([render/2, 
         render/3, 
         render_str/2, 
         render_str/3, 
         render_bin/2,
         render_bin/3]).

%%--------------------------------------------------------------------
%% @spec render_str(compiled(), data(), options()) -> string()
%% @doc Calls render/3 and converts the result to string.
%% @end
%%--------------------------------------------------------------------
render_str(Compiled, Data, Options) ->
    to_str(render(Compiled, Data, Options)).

%%--------------------------------------------------------------------
%% @spec render_str(compiled(), data()) -> string()
%% @doc Calls render/2 and converts the result to string.
%% @end
%%--------------------------------------------------------------------
render_str(Compiled, Data) ->
    to_str(render(Compiled, Data)).

%%--------------------------------------------------------------------
%% @spec render_bin(compiled(), data(), options()) -> binary()
%% @doc Calls render/3 and converts the result to binary.
%% @end
%%--------------------------------------------------------------------
render_bin(Compiled, Data, Options) ->
    to_bin(render(Compiled, Data, Options)).
    
%%--------------------------------------------------------------------
%% @spec render_bin(compiled(), data()) -> binary()
%% @doc Calls render/2 and converts the result to binary.
%% @end
%%--------------------------------------------------------------------
render_bin(Compiled, Data) ->
    to_bin(render(Compiled, Data)).
    
%%--------------------------------------------------------------------
%% @spec render(compiled(), data(), options()) -> string()
%% @doc Calls render/2 passing options in the data.
%% @end
%%--------------------------------------------------------------------
render(Compiled, Data, Options) when is_list(Data) ->
    render(Compiled, [{options, Options}|Data]);
render(Compiled, Data, Options) ->  %% Data is a sgte_dict
    render(Compiled, sgte_dict:store(options, Options, Data)).

%%--------------------------------------------------------------------
%% @spec render(compiled(), data()) -> string()
%% @doc Renders the compiled template and returns it
%% @end
%%--------------------------------------------------------------------
render(Compiled, Data) when is_list(Data) ->
    render(Compiled, sgte_dict:from_list(Data));
render(Compiled, Data) when is_function(Compiled) ->
    render_final(Compiled, Data);
render(Compiled, Data) ->
    [render_element(X, Data) || X <- Compiled].

%%--------------------------------------------------------------------
%% @spec render1(compiled(), data(), {Key, Value}) -> string()
%% @doc Renders the compiled template and returns it. 
%% Used to render map tokens.
%% @end
%%--------------------------------------------------------------------
render1(Compiled, Data, Attr) when is_list(Attr) ->
    Data1 = sgte_dict:merge(fun(_K, _V1, V2) -> V2 end, Data, sgte_dict:from_list(Attr)),
    [render_element(X, Data1) || X <- Compiled];
render1(Compiled, Data, Attr) when is_function(Compiled) ->
    {K, V} = Attr,
    render_final(Compiled, sgte_dict:store(K, V, Data));
render1(Compiled, Data, Attr) ->
    {K, V} = Attr,
    Data1 = sgte_dict:store(K, V, Data),
    [render_element(X, Data1) || X <- Compiled].

    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @spec to_str(deep_list()) -> string()
%% @doc Gets a deep list with mixed strings and binaries. Converts
%% everything to string and returns the flattened list.
%% @end
%%--------------------------------------------------------------------
to_str(DeepL) ->
    ToStr = fun(El) when is_binary(El) ->
                    binary_to_list(El);
               (El) ->
                    El
             end,
    lists:flatten(lists:map(ToStr, lists:flatten(DeepL))).

%%--------------------------------------------------------------------
%% @spec to_bin(deep_list()) -> string()
%% @doc Gets a deep list with mixed strings and binaries. Converts
%% everything to binary and returns the flattened list.
%% @end
%%--------------------------------------------------------------------
to_bin(DeepL) ->
    ToBin = fun(El) when is_list(El) ->
                    list_to_binary(El);
               (El) ->
                    El
            end,
    lists:flatten(lists:map(ToBin, DeepL)).

%%--------------------------------------------------------------------
%% @spec render_final(term(), data(), test) -> term()
%% @doc Render on final attribute. Called in the render of an if test.
%% @end
%%--------------------------------------------------------------------
render_final(Attribute, Data, test) when is_function(Attribute) ->
    Attribute(Data);
render_final(Attribute, _Data, test) ->
    Attribute.

%%--------------------------------------------------------------------
%% @spec render_final(term(), data()) -> string()
%% @doc Render on final attribute. If it's a function call it on Data,
%% else returns attribute.
%% @end
%%--------------------------------------------------------------------
render_final(Attribute, Data) when is_function(Attribute) ->
    render_final(Attribute(Data));
render_final(Attribute, _Data) ->
    render_final(Attribute).

%%--------------------------------------------------------------------
%% @spec render_final(term()) -> string()|{error, Reason}
%% @doc Render on final attribute. 
%% @end
%%--------------------------------------------------------------------
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
%% @spec render_element(token(), data()) -> string()|{error, Reason}
%% @doc Render a token from the parsed template.
%% @end
%%--------------------------------------------------------------------
render_element({attribute, AttrList, Line}, Data) ->
    case get_value(AttrList, Data, attribute) of
	{error, X} ->
	    on_error(fun empty_string/0, Data, X, Line);
        V ->
            printable(V, fun(El) -> render_final(El, Data) end)
    end;
render_element({join, {Separator, Term}, Line}, Data) ->
    case get_value(Term, Data, join) of
	{error, X} ->
	    on_error(fun empty_string/0, Data, X, Line);
	ValueList ->
	    Concat = lists:flatten([printable(X, fun(El) -> El end)++Separator 
                                    || X <- ValueList]),
            case Concat of
                [] -> Concat;
                _ ->
                    string:sub_string(Concat, 1, length(Concat)-length(Separator))
            end
    end;

render_element({include, Tmpl, Line}, Data) -> %% include template passing all data
    case get_value(Tmpl, Data, include) of
	{error, X} ->
	    on_error(fun empty_string/0, Data, X, Line);
	Compiled ->
	    render(Compiled, Data)
    end;

render_element({apply, {Callable, VarList}, Line}, Data) -> %% apply first element to VarList
    case get_value(Callable, Data, apply) of
	{error, X} ->
	    on_error(fun empty_string/0, Data, X, Line);
	ToCall ->
	    case get_value(VarList, Data, apply) of
		{error, X} ->
		    on_error(fun empty_string/0, Data, X, Line);
		Value ->
		    printable(ToCall(Value), fun(El) -> El end)
	    end
    end;

render_element({map, {Tmpl, Term}, Line}, Data) ->
    case {get_value(Tmpl, Data, map), get_value(Term, Data, map)} of
	{{error, X}, _} ->
	    on_error(fun empty_string/0, Data, X, Line);
	{_, {error, X}} ->
	    on_error(fun empty_string/0, Data, X, Line);
	{CT, ValueList} ->
	    [render1(CT, Data, V) || V <- ValueList]
    end;
render_element({mapl, {Tmpl, Term}, Line}, Data) ->
    case {get_value(Tmpl, Data, mapl), get_value(Term, Data, mapl)} of
	{{error, X}, _} ->
	    on_error(fun empty_string/0, Data, X, Line);
	{_, {error, X}} ->
	    on_error(fun empty_string/0, Data, X, Line);
	{CT, ValueList} ->
	    [render1(CT, Data, {attr, V}) || V <- ValueList]
    end;
render_element({mapj, {Tmpl, Term, Separator}, Line}, Data) ->
    case {get_value(Tmpl, Data, map), 
	  get_value(Term, Data, mapj), 
	  get_value(Separator, Data, mapj)} of
	{{error, X}, _, _} ->
	    on_error(fun empty_string/0, Data, X, Line);
	{_, {error, X}, _} ->
	    on_error(fun empty_string/0, Data, X, Line);
	{_, _, {error, X}} ->
	    on_error(fun empty_string/0, Data, X, Line);
	{CT, ValueList, CSep} ->
	    MappedVal = [render1(CT, Data, V) || V <- ValueList],
	    Concat = lists:flatten([X++CSep || X <- MappedVal]),
	    Value = string:sub_string(Concat, 1, length(Concat)-length(CSep)),
	    Value
    end;
render_element({mmap, {Tmpl, Term}, Line}, Data) ->
    case get_value(Term, Data, mmap) of
	{error, X} ->
	    on_error(fun empty_string/0, Data, X, Line);
	ValueList ->
	    ExtractTmpl = fun(El, Acc) ->
				  Compiled = get_value(El, Data, map),
				  [Compiled|Acc]
			  end,
	    CompiledList = lists:reverse(lists:foldl(ExtractTmpl, [], Tmpl)),
	    % Zipped is a tuple list: [{tmpl1, val1}, {tmpl2, val2},{tmpl1, val3} ...]
	    Zipped = group(CompiledList, ValueList), 
	    [render1(CT, Data, V) || {CT, V} <- Zipped]
    end;
render_element({imap, {[TmplList], Term}, Line}, Data) ->
    case get_value(Term, Data, imap) of
	{error, X} ->
	    on_error(fun empty_string/0, Data, X, Line);
	ValueList ->
	    [render1(TmplList, Data, V) || V <- ValueList]
    end;
render_element({gettext, Key, Line}, Data) ->
    case gettext_lc(options(Data)) of
	{error, X} ->
	    on_error(fun () -> Key end, Data, X, Line);
	LC ->
	    case catch gettext:key2str(Key, LC) of
		Translation when list(Translation) -> Translation;
		_ -> Key
	    end 
    end;
render_element({ift, {{attribute, Test}, Then, Else}, Line}, Data) ->
    case get_value(Test, Data, ift) of
	{error, X} -> % test not found -> false
	    on_error(fun () -> render(Else, Data) end, 
				Data, X, Line);
		    
	TestP ->
	    case render_final(TestP, Data, test) of
		false ->
		    render(Else, Data);
		[] ->
		    render(Else, Data);
		{} ->
		    render(Else, Data);
		0 ->
		    render(Else, Data);
		_ ->
		    render(Then, Data)
	    end
    end;
render_element({ift, {{attribute, Test}, Then}, Line}, Data) ->
    case get_value(Test, Data, ift) of
	{error, X} -> % test not found -> false
	    on_error(fun empty_string/0, Data, X, Line);
	TestP ->
	    case render_final(TestP, Data, test) of
		false ->
		    [];
		[] ->
		    [];
		{} ->
		    [];
		0 ->
		    [];
		_ ->
		    render(Then, Data)
	    end
    end;
render_element(Term, _Data) ->
    render_final(Term).


%%--------------------------------------------------------------------
%% @spec on_error(Continuation::function(), 
%%                Data::data(), 
%%                ErrReason,
%%                Line::int()) -> string()
%% @doc In case of an error due to a value not passed in data 
%% manage the result based on options. If the quiet option is set 
%% returns Continuation(), else a warning message is returned.
%% @end
%%--------------------------------------------------------------------
on_error(Continuation, Data, ErrReason, Line) ->
    case is_quiet(options(Data)) of
	true ->
	    Continuation();
	false ->
	    render_error({warning, ErrReason, {line, Line}})
    end.

%%--------------------------------------------------------------------
%% @spec empty_string() -> []
%% @doc Returns an empty string.
%% @end
%%--------------------------------------------------------------------
empty_string() ->    
    [].

%%--------------------------------------------------------------------
%% @spec render_error(error_or_warning()) -> string()
%% @doc Returns an error or warning message.
%% @end
%%--------------------------------------------------------------------
render_error({warning, {TmplEl, Key, not_found}, {line, LineNo}}) ->
    io_lib:format("[SGTE Warning: template: ~p - key ~p not found on line ~p]", [TmplEl, Key, LineNo]);
render_error({error, {TmplEl, Key, not_found}, {line, LineNo}}) ->
    io_lib:format("[SGTE Error: template: ~p - key ~p not found on line ~p]", [TmplEl, Key, LineNo]);
%% Render an error in the data type format
render_error({error, {Term, DataType, invalid_data}}) ->
    io_lib:format("[SGTE Error: invalid data type: ~p is a ~p. String expected]", [Term, DataType]);
%% Render a WarningMsg passed as a string
render_error({warning, {TmplEl, WarningMsg}, {line, LineNo}}) when is_list(WarningMsg) ->
    io_lib:format("[SGTE Warning: ~p ~s on line ~p]", [TmplEl, WarningMsg, LineNo]);
%% Render an ErrMsg passed as a string
render_error({error, {TmplEl, ErrMsg}, {line, LineNo}}) when is_list(ErrMsg) ->
    io_lib:format("[SGTE Error: ~p ~s on line ~p]", [TmplEl, ErrMsg, LineNo]).

%%--------------------------------------------------------------------
%% Utilities
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @spec get_value(Key::atom(), 
%%                 Dict::sgte_dict(), 
%%                 Where::atom()) -> Value|{error, Reason}
%% @doc get Key from Dict. If Key is not found returns an error.
%% Where is used to return a more desciptive error.
%% @end
%%--------------------------------------------------------------------
get_value(Key, Dict, Where) when is_atom(Key) ->
    case sgte_dict:find(Key, Dict) of
	{ok, V} ->
	    V;
	error ->
	    {error, {Where, Key, not_found}}
    end;
get_value(KeyList, Dict, Where) ->
    case sgte_dict:rfind(KeyList, Dict) of
	{ok, V} ->
	    V;
	{error, Key} ->
	    {error, {Where, Key, not_found}}
    end.

%%--------------------------------------------------------------------
%% @spec group(L1::list(), L2::list()) -> list()
%% @doc Build a list of tuples [{El1, El2}].
%% If length(L1) &lt; length(L2) when elements of L1 end restart with the complete list L1
%% If length(L1) &gt; length(L2) elements in L1 whit index > length(L2) are ignored
%% e.g.
%% L1 = [1, 2, 3], L2 = [a, b] =&gt; [{1, a}, {2, b}]
%% L1 = [1, 2], L2 = [a, b, c, d, e] =&gt; [{1, a}, {2, b}, {1, c}, {2, d}, {1, e}]
%% @end
%%--------------------------------------------------------------------
group(L1, L2) ->
    group(L1, L2, L1, []).
group(_, [], _Recurring, Result) ->
    lists:flatten(lists:reverse(Result));
group([], [H2|R2], Recurring, Result) ->
    group(Recurring, [H2|R2], Recurring, Result);
group([H1|R1], [H2|R2], Recurring, Result) ->
    group(R1, R2, Recurring, [{H1, H2}|Result]).

%%--------------------------------------------------------------------
%% @spec options(data()) -> [option()]|[]
%% @doc Get Options from Data Dict if present.
%% @end
%%--------------------------------------------------------------------
options(Data) ->
    case sgte_dict:find(options, Data) of
	{ok, Options} ->
	    Options;
	_ ->
	    []
    end.

%%--------------------------------------------------------------------
%% @spec is_quiet(options()) -> bool()
%% @doc Search quiet option in the Option List.
%% @end
%%--------------------------------------------------------------------
is_quiet(Options) ->
    lists:member(quiet, Options).

%%--------------------------------------------------------------------
%% @spec gettext_lc(options()) -> LC::string()
%% @doc Search gettext_lc option Options List. 
%% This is the language used to be used by gettext in the rendering.
%% @end
%%--------------------------------------------------------------------
gettext_lc(Options) ->    
    case lists:keysearch(gettext_lc, 1, Options) of 
	{value, {gettext_lc, LC}} ->
	    LC;
	false ->
	    {error, {gettext, gettext_lc, not_found}}
    end.

%%--------------------------------------------------------------------
%% @spec printable(Term::term(), Else::function()) -> string()
%% @doc Tries to convert an erlang term to string
%% @end
%%--------------------------------------------------------------------
printable(Term, _Else) when is_integer(Term) ->
    integer_to_list(Term);
printable(Term, _Else) when is_float(Term) ->
    float_to_list(Term);
printable(Term, _Else) when is_atom(Term) ->
    atom_to_list(Term);
printable(Term, _Else) when is_binary(Term) ->
    binary_to_list(Term);
printable(Term, Else) ->
    Else(Term).
