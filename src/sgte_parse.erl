%%%-------------------------------------------------------------------
%%% File    : sgte_parse.erl
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
%%% Created : 25 Mar 2007 by filippo pacini <pacini@sgconsulting.it>
%%%-------------------------------------------------------------------
-module(sgte_parse).

-author("$Author$").
-version("$Rev$").
-date("$Date$").

%% API
-export([parse/1, gettext/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
parse(Template) ->
    parse(Template, [], 1).

parse([], Parsed, _Line) ->
    {ok, lists:reverse(Parsed)};
parse("$include"++T, Parsed, Line) ->
    P = and_parser([fun strip_blank/1, 
		    until(fun is_dollar/1)]),
    case P(T) of
	{ok, [Token], LinesParsed, Rest} ->
	    parse(Rest, [{include, Token, Line}|Parsed], Line+LinesParsed);
	{error, Reason} -> 
	    {error, {include, Reason, Line}}
    end;
parse("$apply"++T, Parsed, Line) ->
    P = and_parser([fun strip_blank/1, 
		    until_space(fun is_blank/1), 
		    until(fun is_dollar/1)]),
    case P(T) of
	{ok, [F, V], LinesParsed, Rest} ->
	    parse(Rest, [{apply, {F, V}, Line}|Parsed], Line+LinesParsed);
	{error, Reason} -> 
	    {error, {apply, Reason, Line}}
    end;
parse("$mapl"++T, Parsed, Line) ->
    P = and_parser([fun strip_blank/1, 
		    until_space(fun is_blank/1), 
		    until(fun is_dollar/1)]),    
    case P(T) of
	{ok, [Tmpl, VList], LinesParsed, Rest} ->
	    parse(Rest, [{mapl, {Tmpl, VList}, Line}|Parsed], Line+LinesParsed);
	{error, Reason} -> 
	    {error, {mapl, Reason, Line}}
    end;
parse("$mapj"++T, Parsed, Line) ->
    P = and_parser([fun strip_blank/1, 
		    until_space(fun is_blank/1), 
		    until_space(fun is_blank/1), 
		    until(fun is_dollar/1)]),    
    case P(T) of
	{ok, [Tmpl, VList, Join], LinesParsed,Rest} ->
	    parse(Rest, [{mapj, {Tmpl, VList, Join}, Line}|Parsed], Line+LinesParsed);
	{error, Reason} -> 
	    {error, {mapj, Reason, Line}}
    end;
parse("$mmap"++T, Parsed, Line) ->
    P = and_parser([fun strip_blank/1, 
		    until_greedy(fun is_blank/1), 
		    until(fun is_dollar/1)]),    
    case P(T) of
	{ok, [TmplL, VList], LinesParsed, Rest} ->
	    parse(Rest, [{mmap, {TmplL, VList}, Line}|Parsed], Line+LinesParsed);
	{error, Reason} -> 
	    {error, {mmap, Reason, Line}}
    end;
parse("$map:"++T, Parsed, Line) ->
    Rules = [fun can_be_blank/1, 
	     parenthesis(fun is_open_bracket/1, fun is_close_bracket/1), 
	     until(fun is_dollar/1)],
    P = and_parser(Rules),    
    case P(T) of
 	{ok, [Inline, VList], LinesParsed, Rest} ->
 	    case parse(Inline) of
 		{error, {Tok, Reason, L}} ->
 		    {error, {Tok, Reason, Line+L}};
 		{ok, InlP} ->
 		    parse(Rest, [{imap, {[InlP], VList}, Line}|Parsed], Line+LinesParsed)
 	    end;
 	{error, Reason} -> 
 	    {error, {imap, Reason, Line}}
    end;
parse("$map"++T, Parsed, Line) ->
    P = and_parser([fun strip_blank/1, 
		    until_space(fun is_blank/1), 
		    until(fun is_dollar/1)]),    
    case P(T) of
	{ok, [Tmpl, VList], LinesParsed, Rest} ->
	    parse(Rest, [{map, {Tmpl, VList}, Line}|Parsed], Line+LinesParsed);
	{error, Reason} -> 
	    {error, {map, Reason, Line}}
    end;
parse("$join:"++T, Parsed, Line) ->
    Rules = [fun can_be_blank/1, 
	     parenthesis(fun is_open_bracket/1, fun is_close_bracket/1), 
	     until(fun is_dollar/1)],
    P = and_parser(Rules),    
    case P(T) of
 	{ok, [Separator, VList], LinesParsed, Rest} ->
	    parse(Rest, [{join, {Separator, VList}, Line}|Parsed], Line+LinesParsed);
 	{error, Reason} -> 
 	    {error, {imap, Reason, Line}}
    end;
parse("$txt:"++T, Parsed, Line) ->
    Rules = [fun can_be_blank/1, 
	     parenthesis(fun is_open_bracket/1, fun is_close_bracket/1), 
	     until(fun is_dollar/1)],
    P = and_parser(Rules),    
    case P(T) of
 	{ok, [Key, ''], LinesParsed, Rest} ->
	    parse(Rest, [{gettext, Key, Line}|Parsed], Line+LinesParsed);
 	{ok, [_Key, Whatever], _LinesParsed, _Rest} ->
 	    {error, {gettext, {Whatever, not_allowed_here}, Line}};
 	{error, Reason} -> 
 	    {error, {gettext, Reason, Line}}
    end;
parse("$if "++T, Parsed, Line) ->
    %% if uses the code from the old version. See if it can be improved
    IfTmpl = collect_ift(T),
    case IfTmpl of
	{error, Reason} ->
	    {error, {ift, Reason, Line}};
	{ift, IfToken, _LinesParsed, Rest1} ->
	    case parse_ift(IfToken) of
		{error, Reason} -> {error, {ift, Reason, Line}};
		{ift, ParsedIf} -> 
		    parse(Rest1, [{ift, ParsedIf, Line}|Parsed], Line)
	    end
    end;
parse([H|T], Parsed, Line) when H == $$ andalso hd(T) == $$ ->
    parse(tl(T), [H|Parsed], Line);
parse([H|T], Parsed, Line) when H == $$ ->
    P =  until(fun is_dollar/1),
    case P(T) of
	{ok, Token, LinesParsed, Rest} ->
	    parse(Rest, [{attribute, Token, Line}|Parsed], Line+LinesParsed);
	{error, Reason} -> 
	    {error, {attribute, Reason, Line}}
    end;
parse([H|T], Parsed, Line) when H == $\\ andalso hd(T) == $$ ->
    parse(tl(T), [hd(T)|Parsed], Line);
parse([H|T], Parsed, Line) when [H] == "\r" andalso hd(T) == "\n" ->
    parse(tl(T), ["\r\n"|Parsed], Line+1);
parse([H|T], Parsed, Line) when [H] == "\r" orelse [H] == "\n" ->
    parse(T, [H|Parsed], Line+1);
parse([H|T], Parsed, Line) ->
    {ok, H1, _L, T1} = simple([H|T]),
    parse(T1, [H1|Parsed], Line).


%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
gettext(Template) ->
    gettext(Template, []).

gettext([], Parsed) ->
    lists:reverse(Parsed);
gettext("$txt:"++T, Parsed) ->
    Rules = [fun can_be_blank/1, 
	     parenthesis(fun is_open_bracket/1, fun is_close_bracket/1), 
	     until(fun is_dollar/1)],
    P = and_parser(Rules),
    case P(T) of
 	{ok, [Key, ''], _LinesParsed, Rest} ->
	    gettext(Rest, [Key|Parsed]);
 	{ok, [_Key, Whatever], _LinesParsed, _Rest} ->
 	    {error, {gettext, {Whatever, not_allowed_here}}};
 	{error, Reason} -> 
 	    {error, {gettext, Reason}}
    end;
gettext([_H|T], Parsed) ->
    gettext(T, Parsed).
   
%%====================================================================
%% Internal functions
%%====================================================================     
%% collect if token till $end if$
collect_ift(Tmpl) ->
    collect_ift(Tmpl, [], {}, 0).

collect_ift([], _Token, _T, _Line) ->
    {error, end_not_found};
collect_ift("\\$"++Rest, Token, T, Line) -> %% Escape sequence for \, $, {, }
    collect_ift(Rest, ["$"|Token], T, Line);
collect_ift("\\\\"++Rest, Token, T, Line) -> %% Escape sequence for \, $, {, }
    collect_ift(Rest, ["\\"|Token], T, Line);
collect_ift("$end if$"++Rest, Token, {Test, Then}, Line) ->
    {ift, {Test, Then, lists:reverse(Token)}, Line, Rest};
collect_ift("$end if$"++Rest, Token, {Test}, Line) ->
    {ift, {Test, lists:reverse(Token)}, Line, Rest};
collect_ift("$else$"++Rest, Token, {Test}, Line) ->
    collect_ift(Rest, [], {Test, lists:reverse(Token)}, Line);
collect_ift("$if "++Rest, Token, {Test}, Line) ->  %% Nested if
    case collect_ift(Rest, [], {}, Line) of
	{ift, InnerIf, LinesParsed, Rest1} ->
	    case parse_ift(InnerIf) of
		{error, Reason} -> 
		    {error, Reason};
		{ift, ParsedIf} -> 
		    collect_ift(Rest1,[{ift, ParsedIf, Line}, lists:reverse(Token)], {Test}, Line+LinesParsed)
	    end;
	{error, E} -> 
	    {error, E}
    end;
collect_ift([H|Rest], Token, {}, Line) when [H] == "$" ->
    collect_ift(Rest, [], {lists:reverse(Token)}, Line);
collect_ift([H|Rest], Token, T, Line) when [H] == "\r" andalso hd(T) == "\n" ->
    collect_ift(Rest, ["\r\n"|Token], T, Line+1);
collect_ift([H|Rest], Token, T, Line) when [H] == "\r" orelse [H] == "\n" ->
    collect_ift(Rest, [H|Token], T, Line+1);
collect_ift([H|Rest], Token, T, Line) ->
    collect_ift(Rest, [H|Token], T, Line).


%% if token parser
parse_ift({Test, Then, Else}) ->
    case {parse(Then), parse(Else)} of
	{{error, Reason1}, _} -> {error, Reason1};
	{_, {error, Reason2}} -> {error, Reason2};
	{{ok, CThen}, {ok, CElse}} -> 
	    {ift, 
	     {{attribute, list_to_atom(string:strip(Test))}, 
	      CThen, CElse}
	    }
    end;
parse_ift({Test, Then}) ->
    case parse(Then) of
	{error, Reason} -> {error, Reason};
	{ok, CThen} ->
	    {ift, {{attribute, list_to_atom(string:strip(Test))}, CThen}}
    end.

%% And parser of Rules
and_parser(Rules) ->
    fun(Tmpl) ->
	    and_parser(Rules, Tmpl, [], 0)
    end.
and_parser([], Tmpl, SoFar, Line) ->
    {ok, lists:reverse(SoFar), Line, Tmpl};
and_parser([Rule|T], Tmpl, SoFar, Line) ->
    case Rule(Tmpl) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Rest, LinesParsed} ->
	    and_parser(T, Rest, SoFar, Line+LinesParsed);
	{ok, Tok, LinesParsed, Rest} ->
	    and_parser(T, Rest, [Tok|SoFar], Line+LinesParsed)
    end.

%% %% Or parser of Rules
%% or_parser(Rules) ->
%%     fun(Tmpl) ->
%% 	    or_parser(Rules, Tmpl)
%%     end.
%% or_parser([], _Tmpl) ->
%%     {error, no_matching_rule};
%% or_parser([Rule|T], Tmpl) ->
%%     case Rule(Tmpl) of
%% 	{error, _Reason} -> %% check next rule
%% 	    or_parser(T, Tmpl);
%% 	{ok, Tok, Rest} -> %match -> return the result
%% 	    {ok, Tok, Rest}
%%     end.

%%
%% Rules
%%
% simple: output whatever it gets in input
simple([H|T]) ->
    {ok, H, 0, T}.

% strip blanks if found.
can_be_blank([H|T]) ->
    case is_blank(H) of
	true ->
	    strip_blank1(T, 0);
	_ ->
	    {ok, [H|T], 0}
    end.
% strip blanks from Tmpl.
strip_blank([H|T]) ->
    case is_blank(H) of
	true ->
	    strip_blank1(T, 0);
	_ ->
	    {error, blank_not_found}
    end.
strip_blank1([], Line) ->
    {ok, [], Line};
strip_blank1([H|T], Line) when [H]=="\r" andalso hd(T)=="\n" ->
    strip_blank1(tl(T), Line+1);
strip_blank1([H|T], Line) when [H]=="\r" orelse [H]=="\n" ->
    strip_blank1(T, Line+1);
strip_blank1([H|T], Line) ->
    case is_blank(H) of
	true ->
	    strip_blank1(T, Line);
	_ ->
	    {ok, [H|T], Line}
    end.

% until predicate P: output what it gets until P(H) is true stripping white spaces.
until(P) ->
    fun (Tmpl) -> until(P, Tmpl, 0, []) end.
until(_P, [], _Line, _Parsed) ->    
    {error, end_not_found};
until(P, [H|T], Line, Parsed) when [H]==" " ->
    until(P, T, Line, Parsed);
until(P, [H|T], Line, Parsed) when [H]=="\r" andalso hd(T)=="\n" ->
    until(P, tl(T), Line+1, Parsed);
until(P, [H|T], Line, Parsed) when [H]=="\n" orelse [H]== "\r" ->
    until(P, T, Line+1, Parsed);
until(P, [H|T], Line, Parsed) ->
    case P(H) of
	true ->
	    {ok, list_to_atom(lists:reverse(Parsed)), Line, T};
	_ ->
	    until(P, T, Line, [H|Parsed])
    end.

% until predicate P without stripping white spaces
until_space(P) ->
    fun (Tmpl) -> until_space(P, Tmpl, 0, []) end.
until_space(_P, [], _Line, _Parsed) ->
    {error, end_not_found};
until_space(P, [H|T], Line, Parsed) when [H]=="\r" andalso hd(T)=="\n" ->
    until_space(P, tl(T), Line+1, ["\r\n"|Parsed]);
until_space(P, [H|T], Line, Parsed) when [H]=="\n" orelse [H]== "\r" ->
    until_space(P, T, Line+1, [H|Parsed]);
until_space(P, [H|T], Line, Parsed) ->
    case P(H) of
	true ->
	    {ok, list_to_atom(lists:reverse(Parsed)), Line, T};
	_ ->
	    until_space(P, T, Line, [H|Parsed])
    end.


% Greedy version: output what it gets until a $ is reached.
% If all template string ends returns an error
until_greedy(P) ->
    fun (Tmpl) -> until_greedy(P, Tmpl, [], 0, []) end.
until_greedy(_P, [], _StrSofFar, _Line, _ResList) ->    
    {error, end_not_found};
until_greedy(_P, [H|T], StrSoFar, Line, ResList) when [H] == "$" ->
    Rest = lists:reverse(StrSoFar) ++ "$",
    {ok, lists:reverse(ResList), Line, lists:flatten([Rest|T])};
until_greedy(P, [H|T], StrSoFar, Line, ResList) when [H]=="\r" andalso hd(T)=="\n" ->
    until_greedy(P, T, StrSoFar, Line+1, ResList);
until_greedy(P, [H|T], StrSoFar, Line, ResList) when [H]=="\n" orelse [H]== "\r" ->
    until_greedy(P, T, StrSoFar, Line+1, ResList);
until_greedy(P, [H|T], StrSoFar, Line, ResList) ->
    case P(H) of
	true ->
	    H1 = list_to_atom(string:strip(lists:reverse(StrSoFar))),
	    until_greedy(P, T, "", Line, [H1|ResList]);
	false ->
	    until_greedy(P, T, [H|StrSoFar], Line, ResList)
    end.

% Match parenthesis: Start and Stop are two predicates which matches
% open and closed parenthesis. Inner parenthesis are coleccted to be parsed later.
parenthesis(Start, Stop) ->
    fun (Tmpl) -> parenthesis(Start, Stop, Tmpl, 0, [], 0) end.

parenthesis(_Start, _Stop, [], _Count, _StrSoFar, _Line) ->
    {error, end_not_found};
parenthesis(Start, Stop, [H|T], Count, StrSoFar, Line) when Count == 0 ->
    case Start(H) of
	true ->
	    parenthesis(Start, Stop, T, Count+1, StrSoFar, Line);
	_ ->
	    {error, start_not_found}
    end;
parenthesis(Start, Stop, [H|T], Count, StrSoFar, Line) when Count > 0 ->
    case Stop(H) of
	true ->
	    Count1 = Count - 1,
	    case Count1 == 0 of
		true ->
		    {ok, lists:reverse(StrSoFar), Line, T};
		_ ->
		    parenthesis(Start, Stop, T, Count1, [H|StrSoFar], Line)
	    end;
	_ ->
	    case Start(H) of
		true ->
		    parenthesis(Start, Stop, T, Count+1, [H|StrSoFar], Line);
		_ ->
		    parenthesis(Start, Stop, T, Count, [H|StrSoFar], Line)
	    end
    end.

%% token(Token) ->
%%     fun(Tmpl) -> token(Tmpl, [], Token) end.

%% token([], _StrSoFar, _Token) ->
%%     {error, token_not_found};
%% token(Tmpl, StrSoFar, Token) ->
%%     case lists:prefix(Token, Tmpl) of
%% 	true ->
%% 	    Rest = lists:nthtail(length(Token), Tmpl),
%% 	    {ok, lists:reverse(StrSoFar), Rest};
%% 	_ ->
%% 	    token(tl(Tmpl), [hd(Tmpl)|StrSoFar], Token)
%%     end.

match_char(Char, Val) ->
    [Char] == Val.

is_blank(C) ->
    match_char(C, " ") 
	orelse match_char(C, "\n")
	orelse match_char(C, "\r").
is_dollar(C) ->
    match_char(C, "$").
is_open_bracket(C) ->
    match_char(C, "{").
is_close_bracket(C) ->
    match_char(C, "}").


