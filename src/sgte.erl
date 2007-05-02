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
-export([compile/1, compile_file/1, render/2, gettext/1]).

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
    sgte_parse:parse(T).

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
render(Compiled, Data) ->
    sgte_render:render(Compiled, Data).

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
gettext(Template) when is_binary(Template) ->
    sgte_parse:gettext(binary_to_list(Template));
gettext(FileName) ->
    case file:read_file(FileName) of
	{ok, Bin} ->
	    gettext(Bin);
	Err -> 
	    Err
    end.
