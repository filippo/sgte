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
-export([parse/1]).

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
parse("$include "++T, Parsed, Line) ->
    case parse_rules([until(fun is_dollar/1)], T, []) of
	{ok, [Token], Rest} ->
	    parse(Rest, [{include, {Token}, Line}|Parsed], Line);
	{error, Reason} -> 
	    {error, {include, Reason, Line}}
    end;
parse("$apply "++T, Parsed, Line) ->
    case parse_rules([until(fun is_space/1), until(fun is_dollar/1)], T, []) of
	{ok, [F, V], Rest} ->
	    parse(Rest, [{apply, {F, V}, Line}|Parsed], Line);
	{error, Reason} -> 
	    {error, {apply, Reason, Line}}
    end;
parse("$map "++T, Parsed, Line) ->
    case parse_rules([until(fun is_space/1), until(fun is_dollar/1)], T, []) of
	{ok, [Tmpl, VList], Rest} ->
	    parse(Rest, [{map, {Tmpl, VList}, Line}|Parsed], Line);
	{error, Reason} -> 
	    {error, {map, Reason, Line}}
    end;
parse("$mapl "++T, Parsed, Line) ->
    case parse_rules([until(fun is_space/1), until(fun is_dollar/1)], T, []) of
	{ok, [Tmpl, VList], Rest} ->
	    parse(Rest, [{mapl, {Tmpl, VList}, Line}|Parsed], Line);
	{error, Reason} -> 
	    {error, {mapl, Reason, Line}}
    end;
parse("$mapj "++T, Parsed, Line) ->
    case parse_rules([until(fun is_space/1), until(fun is_space/1), until(fun is_dollar/1)], T, []) of
	{ok, [Tmpl, VList, Join], Rest} ->
	    parse(Rest, [{mapj, {Tmpl, VList, Join}, Line}|Parsed], Line);
	{error, Reason} -> 
	    {error, {mapj, Reason, Line}}
    end;
parse("$mmap "++T, Parsed, Line) ->
    case parse_rules([until_greedy(fun is_space/1), until(fun is_dollar/1)], T, []) of
	{ok, [TmplL, VList], Rest} ->
	    parse(Rest, [{mmap, {TmplL, VList}, Line}|Parsed], Line);
	{error, Reason} -> 
	    {error, {mmap, Reason, Line}}
    end;
parse("$join "++T, Parsed, Line) ->
    Rules = [parenthesis(fun is_open_bracket/1, fun is_close_bracket/1), 
	     until(fun is_dollar/1)],
     case parse_rules(Rules, T, []) of
 	{ok, [Separator, VList], Rest} ->
	     parse(Rest, [{join, {Separator, VList}, Line}|Parsed], Line);
 	{error, Reason} -> 
 	    {error, {imap, Reason, Line}}
     end;
parse("$map:"++T, Parsed, Line) ->
    Rules = [parenthesis(fun is_open_bracket/1, fun is_close_bracket/1), 
	     until(fun is_dollar/1)],
     case parse_rules(Rules, T, []) of
 	{ok, [Inline, VList], Rest} ->
 	    case parse(Inline) of
 		{error, {Tok, Reason, L}} ->
 		    {error, {Tok, Reason, Line+L}};
 		{ok, P} ->
		    L = element(size(P), P),
 		    parse(Rest, [{imap, {[P], VList}, Line}|Parsed], Line+L)
 	    end;
 	{error, Reason} -> 
 	    {error, {imap, Reason, Line}}
     end;
parse("$if "++T, Parsed, Line) ->
    Rules = [until(fun is_dollar/1), token("$else$"), token("$endif$")],
    case parse_rules(Rules, T, []) of
 	{ok, Test, Rest} ->
	    parse(Rest, [{ift, Test, Line}|Parsed], Line);
 	{error, Reason} -> 
 	    {error, {ift, Reason, Line}}
     end;
parse([H|T], Parsed, Line) when H == $$ andalso hd(T) == $$ ->
    parse(tl(T), [H|Parsed], Line);
parse([H|T], Parsed, Line) when H == $$ ->
    case parse_rules([until(fun is_dollar/1)], T, []) of
	{ok, [Token], Rest} ->
	    parse(Rest, [{attribute, Token, Line}|Parsed], Line);
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
    {ok, H1, T1} = simple([H|T], []),
    parse(T1, [H1|Parsed], Line).

   
%%====================================================================
%% Internal functions
%%====================================================================     
parse_rules([], Tmpl, SoFar) ->
    {ok, lists:reverse(SoFar), Tmpl};
parse_rules([Rule|T], Tmpl, SoFar) ->
    case Rule(Tmpl) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Tok, Rest} ->
	    parse_rules(T, Rest, [Tok|SoFar])
    end.

%%
%% Rules
%%

% simple: output whatever it gets in input
simple([H|T], []) ->
    {ok, H, T}.

% until predicate P: output what it gets until P(H) is true.
% If all template string ends returns an error
until(P) ->
    fun (Tmpl) -> until(P, Tmpl, []) end.
until(_P, [], _Parsed) ->    
    {error, end_not_found};
until(P, [H|T], Parsed) ->
    case P(H) of
	true ->
	    {ok, list_to_atom(string:strip(lists:reverse(Parsed))), T};
	_ ->
	    until(P, T, [H|Parsed])
    end.


% Greedy version: output what it gets until a $ is reached.
% If all template string ends returns an error
until_greedy(P) ->
    fun (Tmpl) -> until_greedy(P, Tmpl, [], []) end.
until_greedy(_P, [], _StrSofFar, _ResList) ->    
    {error, end_not_found};
until_greedy(_P, [H|T], StrSoFar, ResList) when [H] == "$" ->
    Rest = lists:reverse(StrSoFar) ++ "$",
    {ok, lists:reverse(ResList), lists:flatten([Rest|T])};
until_greedy(P, [H|T], StrSoFar, ResList) ->
    case P(H) of
	true ->
	    H1 = list_to_atom(string:strip(lists:reverse(StrSoFar))),
	    until_greedy(P, T, "", [H1|ResList]);
	false ->
	    until_greedy(P, T, [H|StrSoFar], ResList)
    end.

% Match parenthesis: Start and Stop are two predicates which matches
% open and closed parenthesis. Inner parenthesis are coleccted to be parsed later.
parenthesis(Start, Stop) ->
    fun (Tmpl) -> parenthesis(Start, Stop, Tmpl, 0, []) end.

parenthesis(_Start, _Stop, [], _Count, _StrSoFar) ->
    {error, end_not_found};
parenthesis(Start, Stop, [H|T], Count, StrSoFar) when Count == 0 ->
    case Start(H) of
	true ->
	    parenthesis(Start, Stop, T, Count+1, StrSoFar);
	_ ->
	    {error, start_not_found}
    end;
parenthesis(Start, Stop, [H|T], Count, StrSoFar) when Count > 0 ->
    case Stop(H) of
	true ->
	    Count1 = Count - 1,
	    case Count1 == 0 of
		true ->
		    {ok, lists:reverse(StrSoFar), T};
		_ ->
		    parenthesis(Start, Stop, T, Count1, [H|StrSoFar])
	    end;
	_ ->
	    case Start(H) of
		true ->
		    parenthesis(Start, Stop, T, Count+1, [H|StrSoFar]);
		_ ->
		    parenthesis(Start, Stop, T, Count, [H|StrSoFar])
	    end
    end.

token(Token) ->
    fun(Tmpl) -> token(Tmpl, [], Token) end.

token([], _StrSoFar, _Token) ->
    {error, token_not_found};
token(Tmpl, StrSoFar, Token) ->
    case lists:prefix(Token, Tmpl) of
	true ->
	    Rest = lists:nthtail(length(Token), Tmpl),
	    {ok, lists:reverse(StrSoFar), Rest};
	_ ->
	    token(tl(Tmpl), [hd(Tmpl)|StrSoFar], Token)
    end.

match_char(Char, Val) ->
    [Char] == Val.

is_space(C) ->
    match_char(C, " ").
is_dollar(C) ->
    match_char(C, "$").
is_open_bracket(C) ->
    match_char(C, "{").
is_close_bracket(C) ->
    match_char(C, "}").


