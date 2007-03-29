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
    parse(Template, []).

parse([], Parsed) ->
    {ok, lists:reverse(Parsed)};
parse("$include "++T, Parsed) ->
    case parse_el([until(fun is_dollar/1)], T, []) of
	{ok, [Token], Rest} ->
	    parse(Rest, [{include, Token}|Parsed]);
	{error, Reason} -> 
	    {error, {include, Reason}}
    end;
parse("$apply "++T, Parsed) ->
    case parse_el([until(fun is_space/1), until(fun is_dollar/1)], T, []) of
	{ok, [F, V], Rest} ->
	    parse(Rest, [{apply, F, V}|Parsed]);
	{error, Reason} -> 
	    {error, {apply, Reason}}
    end;
parse("$map "++T, Parsed) ->
    case parse_el([until(fun is_space/1), until(fun is_dollar/1)], T, []) of
	{ok, [Tmpl, VList], Rest} ->
	    parse(Rest, [{map, Tmpl, VList}|Parsed]);
	{error, Reason} -> 
	    {error, {map, Reason}}
    end;
parse("$mapl "++T, Parsed) ->
    case parse_el([until(fun is_space/1), until(fun is_dollar/1)], T, []) of
	{ok, [Tmpl, VList], Rest} ->
	    parse(Rest, [{mapl, Tmpl, VList}|Parsed]);
	{error, Reason} -> 
	    {error, {mapl, Reason}}
    end;
parse("$mapj "++T, Parsed) ->
    case parse_el([until(fun is_space/1), until(fun is_space/1), until(fun is_dollar/1)], T, []) of
	{ok, [Tmpl, VList, Join], Rest} ->
	    parse(Rest, [{mapj, Tmpl, VList, Join}|Parsed]);
	{error, Reason} -> 
	    {error, {mapj, Reason}}
    end;
parse("$mmap "++T, Parsed) ->
    case parse_el([until_greedy(fun is_space/1), until(fun is_dollar/1)], T, []) of
	{ok, [TmplL, VList], Rest} ->
	    parse(Rest, [{mmap, TmplL, VList}|Parsed]);
	{error, Reason} -> 
	    {error, {mmap, Reason}}
    end;
parse("$map:"++T, Parsed) ->
    Rules = [parenthesis(fun is_open_bracket/1, fun is_close_bracket/1), 
	     until(fun is_dollar/1)],
     case parse_el(Rules, T, []) of
 	{ok, [Inline, VList], Rest} ->
 	    case parse(Inline, []) of
 		{error, Reason} ->
 		    {error, Reason};
 		{ok, P} ->
 		    parse(Rest, [{imap, [P], VList}|Parsed])
 	    end;
 	{error, Reason} -> 
 	    {error, {imap, Reason}}
     end;
parse([H|T], Parsed) when H == $$ andalso hd(T) == $$ ->
    parse(tl(T), [H|Parsed]);
parse([H|T], Parsed) when H == $$ ->
    case parse_el([until(fun is_dollar/1)], T, []) of
	{ok, [Token], Rest} ->
	    parse(Rest, [{attribute, Token}|Parsed]);
	{error, Reason} -> 
	    {error, {attribute, Reason}}
    end;
parse([H|T], Parsed) when H == $\\ andalso hd(T) == $$ ->
    parse(tl(T), [hd(T)|Parsed]);
parse([H|T], Parsed) ->
    {ok, H1, T1} = simple([H|T], []),
    parse(T1, [H1|Parsed]).

   
%%====================================================================
%% Internal functions
%%====================================================================     
parse_el([], Tmpl, SoFar) ->
    {ok, lists:reverse(SoFar), Tmpl};
parse_el([Rule|T], Tmpl, SoFar) ->
    case Rule(Tmpl) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Tok, Rest} ->
	    parse_el(T, Rest, [Tok|SoFar])
    end.

simple([H|T], []) ->
    {ok, H, T}.


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

if_then_else(If, Else, EndIf) ->
    fun (Tmpl) -> if_then_else(If, Else, EndIf, Tmpl, 0, []) end.

if_then_else(_If, _Else, _EndIf, [], _Count, _StrSoFar) ->
    {error, end_not_found};
if_then_else(If, Else, EndIf, [H|T], Count, StrSoFar) when Count == 0 ->
    case If(H) of
	true ->
	    if_then_else(If, Else, EndIf, T, Count+1, StrSoFar);
	_ ->
	    {error, start_not_found}
    end;
if_then_else(If,Else, EndIf, [H|T], Count, StrSoFar) when Count > 0 ->
    case EndIf(H) of
	true ->
	    Count1 = Count - 1,
	    case Count1 == 0 of
		true ->
		    {ok, lists:reverse(StrSoFar), T};
		_ ->
		    if_then_else(If, Else, EndIf, T, Count1, [H|StrSoFar])
	    end;
	_ ->
	    case If(H) of
		true ->
		    if_then_else(If, Else, EndIf, T, Count+1, [H|StrSoFar]);
		_ ->
		    if_then_else(If, Else, EndIf, T, Count, [H|StrSoFar])
	    end
    end.



match_char(Char, Val) ->
    [Char] == Val.

is_space(C) ->
    match_char(C, " ").
is_dollar(C) ->
    match_char(C, "$").
is_close_bracket(C) ->
    match_char(C, "}").
is_open_bracket(C) ->
    match_char(C, "{").
