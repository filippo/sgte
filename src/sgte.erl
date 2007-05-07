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
%%% @doc 
%%% <p>The <em>SGTE</em> module is a library implementing a Template Engine
%%% The template system is inspired on <a href="http://www.stringtemplate.org">String Template</a>
%%% </p><p>
%%% The use of the Engige is as simple as (from the command line):
%%% <pre>
%%% > {ok, Compiled} = sgte:compile(TmplStr),
%%% > sgte:render(Compiled, Data).
%%% or:
%%% > {ok, Compiled} = sgte:compile_file(FileName),
%%% > sgte:render(Compiled, Data).
%%% </pre>
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

%% API
-export([compile/1, compile_file/1, render/2, render/3, gettext_strings/1]).

%%yaws_tei is not in a public release yet -behaviour(yaws_tei).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec compile(T::template()) -> {ok, C::compiled_tmpl()} | {error,Reason}
%%
%%   @type template() = string() | binary(). Template to compile (string or binary)
%%   @type compiled_tmpl() = [char()|token()]
%%          token() = tupe().
%%
%% @doc Compiles the template string T and returns the compiled 
%% template or an error.
%% @end
%%--------------------------------------------------------------------
compile(T) when is_binary(T) ->
    sgte_parse:parse(binary_to_list(T));
compile(T) when is_list(T) ->
    sgte_parse:parse(T).

%%--------------------------------------------------------------------
%% @spec compile_file(FileName) -> {ok, C::compiled_tmpl()} | {error,Reason}
%%
%% @doc Compiles the template file FileName and returns the compiled 
%% template or an error.
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
%% @spec render(C::compiled_tmpl(), 
%%              Data::tmpl_data(), 
%%              Options::options()) -> string()
%%
%% @type tmpl_data() = [tuple()]|dict()
%% @type options() = [options()]
%%       option()  = strict|{gettext_lc, string()}.
%%
%% @doc Renders the compiled template.
%% @end
%%--------------------------------------------------------------------
render(Compiled, Data, Options) ->
    sgte_render:render(Compiled, Data, Options).

%%--------------------------------------------------------------------
%% @spec render(C::compiled_tmpl(), Data::tmpl_data()) -> string()
%%
%% @type tmpl_data() = [tuple()]|dict()
%%
%% @doc Renders the compiled template.
%% @end
%%--------------------------------------------------------------------
render(Compiled, Data) ->
    sgte_render:render(Compiled, Data).

%%--------------------------------------------------------------------
%% @spec gettext_strings(T::template()) -> [gettext_tuple()]
%%
%% @type gettext_tuple() = {Key, LineNo}
%%
%% @doc Extracts from template T the list of gettext keys 
%% with associated line numbers.
%% This is a utility function to use in cojunction with gettext
%% to create initial .po files.
%% @end
%%--------------------------------------------------------------------
gettext_strings(Template) when is_binary(Template) ->
    sgte_parse:gettext_strings(binary_to_list(Template));
gettext_strings(FileName) ->
    case file:read_file(FileName) of
	{ok, Bin} ->
	    gettext_strings(Bin);
	Err -> 
	    Err
    end.
