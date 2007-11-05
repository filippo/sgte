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
%%% @doc 
%%% <p>Parses a template file or string and returns the 
%%% compiled template.</p>
%%%
%%% <p>This module is not meant to be used directly. It's called 
%%% through the interface of the sgte module.</p>
%%% @end
%%%
%%% Created : 25 Mar 2007 by filippo pacini <pacini@sgconsulting.it>
%%%-------------------------------------------------------------------
-module(sgte_parse).

-ifdef(ERLHIVE).
-import(.lists).    % erlhive uses package notation
-import(.string).   % ditto
-endif.

%% API
-export([parse/1, gettext_strings/1]).

-define(KEYWORD_START, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_").


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec parse(T::template()) -> {ok, C::compiled()} | {error,Reason}
%%
%%   @type template() = string() | binary(). Template to parse
%%   @type compiled() = [char()|token()]
%%          token() = tupe().
%%
%% @doc Parse the template string T and returns the compiled 
%% template or an error.
%% @end
%%--------------------------------------------------------------------
parse(Template) ->
    parse(Template, [], [], 1).

parse([], Parsed, [], _Line) ->
    {ok, lists:reverse(Parsed)};
parse([], Parsed, Acc, _Line) ->
    Raw = list_to_binary(lists:reverse(Acc)),
    {ok, lists:reverse([Raw | Parsed])};
parse("$$" ++ T, Parsed, Acc, Line) ->
    parse(T, Parsed, "$" ++ Acc, Line);
parse("$" ++ T, Parsed, Acc, Line) ->
    case parse_key(T, Line) of
        {ok, Keyword, LinesParsed, Rest} ->
            case Acc of
                [] ->
                    parse(Rest, [Keyword|Parsed], [], Line+LinesParsed);
                _ ->
                    Raw = list_to_binary(lists:reverse(Acc)),
                    parse(Rest, [Keyword,Raw|Parsed], [], Line+LinesParsed)
            end;
        false ->
            parse(T, Parsed, "$" ++ Acc, Line);
        {error, Reason} -> 
            {error, {attribute, Reason, Line}}
    end;
parse([H|T], Parsed, Acc, Line) when H == $\\ andalso hd(T) == $$ ->
    parse(tl(T), Parsed, [hd(T)|Acc], Line);
parse([H|T], Parsed, Acc, Line) when [H] == "\r" andalso hd(T) == "\n" ->
    parse(tl(T), Parsed, ["\r\n"|Acc], Line+1);
parse([H|T], Parsed, Acc, Line) when [H] == "\r" orelse [H] == "\n" ->
    parse(T, Parsed, [H|Acc], Line+1);
parse([{ift, _, ParsedLines}=H|T], Parsed, Acc, Line) ->
    case Acc of 
        [] ->
            parse(T, [H|Parsed], Acc, Line+ParsedLines);
        _ ->
            parse(T, [H|[list_to_binary(lists:reverse(Acc))|Parsed]], [], Line+ParsedLines)
    end;
parse([H|T], Parsed, Acc, Line) ->
    case simple([H|T]) of
        {ok, [], _L, T1} ->
            parse(T1, Parsed, Acc, Line);
        {ok, H1, _L, T1} ->
            parse(T1, Parsed, [H1|Acc], Line)
    end.


parse_key("include"++T, Line) ->
    P = and_parser([fun strip_blank/1, 
		    until(fun is_dollar/1)]),
    case P(T) of
	{ok, [Token], LinesParsed, Rest} ->
	    {ok, {include, Token, Line}, LinesParsed, Rest};
	{error, Reason} -> 
	    {error, {include, Reason, Line}}
    end;
parse_key("apply"++T, Line) ->
    P = and_parser([fun strip_blank/1, 
		    until_space(fun is_blank/1), 
		    until(fun is_dollar/1)]),
    case P(T) of
	{ok, [F, V], LinesParsed, Rest} ->
	    {ok, {apply, {F, V}, Line}, LinesParsed, Rest};
	{error, Reason} -> 
	    {error, {apply, Reason, Line}}
    end;
parse_key("mapl"++T, Line) ->
    P = and_parser([fun strip_blank/1, 
		    until_space(fun is_blank/1), 
		    until(fun is_dollar/1)]),    
    case P(T) of
	{ok, [Tmpl, VList], LinesParsed, Rest} ->
	    {ok, {mapl, {Tmpl, VList}, Line}, LinesParsed, Rest};
	{error, Reason} -> 
	    {error, {mapl, Reason, Line}}
    end;
parse_key("mapj"++T, Line) ->
    P = and_parser([fun strip_blank/1, 
		    until_space(fun is_blank/1), 
		    until_space(fun is_blank/1), 
		    until(fun is_dollar/1)]),    
    case P(T) of
	{ok, [Tmpl, VList, Join], LinesParsed,Rest} ->
	    {ok, {mapj, {Tmpl, VList, Join}, Line}, LinesParsed, Rest};
	{error, Reason} -> 
	    {error, {mapj, Reason, Line}}
    end;
parse_key("mmap"++T, Line) ->
    P = and_parser([fun strip_blank/1, 
		    until_greedy(fun is_blank/1), 
		    until(fun is_dollar/1)]),    
    case P(T) of
	{ok, [TmplL, VList], LinesParsed, Rest} ->
            {ok, {mmap, {TmplL, VList}, Line}, LinesParsed, Rest};
	{error, Reason} -> 
	    {error, {mmap, Reason, Line}}
    end;
parse_key("map:"++T, Line) ->
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
 		    {ok, {imap, {[InlP], VList}, Line}, LinesParsed, Rest}
 	    end;
 	{error, Reason} -> 
 	    {error, {imap, Reason, Line}}
    end;
parse_key("map"++T, Line) ->
    P = and_parser([fun strip_blank/1, 
		    until_space(fun is_blank/1), 
		    until(fun is_dollar/1)]),    
    case P(T) of
	{ok, [Tmpl, VList], LinesParsed, Rest} ->
	    {ok, {map, {Tmpl, VList}, Line}, LinesParsed, Rest};
	{error, Reason} -> 
	    {error, {map, Reason, Line}}
    end;
parse_key("join:"++T, Line) ->
    Rules = [fun can_be_blank/1, 
	     parenthesis(fun is_open_bracket/1, fun is_close_bracket/1), 
	     until(fun is_dollar/1)],
    P = and_parser(Rules),    
    case P(T) of
 	{ok, [Separator, VList], LinesParsed, Rest} ->
	    {ok, {join, {Separator, VList}, Line}, LinesParsed, Rest};
 	{error, Reason} -> 
 	    {error, {join, Reason, Line}}
    end;
parse_key("txt:"++T, Line) ->
    Rules = [fun can_be_blank/1, 
	     parenthesis(fun is_open_bracket/1, fun is_close_bracket/1), 
	     until(fun is_dollar/1)],
    P = and_parser(Rules),
    case P(T) of
 	{ok, [Key, ""], LinesParsed, Rest} ->
	    {ok, {gettext, Key, Line}, LinesParsed, Rest};
 	{ok, [_Key, Whatever], _LinesParsed, _Rest} ->
 	    {error, {gettext, {Whatever, not_allowed_here}, Line}};
 	{error, Reason} -> 
 	    {error, {gettext, Reason, Line}}
    end;
parse_key("if "++T, Line) ->
    %% if uses the code from the old version. See if it can be improved
    IfTmpl = collect_ift(T),
    case IfTmpl of
	{error, Reason} ->
	    {error, {ift, Reason, Line}};
	{ift, IfToken, _LinesParsed, Rest1} ->
	    case parse_ift(IfToken) of
		{error, Reason} -> {error, {ift, Reason, Line}};
		{ift, ParsedIf} -> 
		    {ok, {ift, ParsedIf, Line}, Line, Rest1}
	    end
    end;
parse_key([H|T], Line) ->
    case lists:member(H, ?KEYWORD_START) of
	true ->
	    P =  until(fun is_dollar/1),
	    case P([H|T]) of
		{ok, Token, LinesParsed, Rest} ->
		    {ok, {attribute, Token, Line}, LinesParsed, Rest};
		{error, Reason} -> 
		    {error, {attribute, Reason, Line}}
	    end;
	false ->
            false
    end.


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
gettext_strings(Template) ->
    gettext_strings(Template, [], 1).

gettext_strings([], Parsed, _L) ->
    lists:reverse(Parsed);
gettext_strings("$txt:"++T, Parsed, L) ->
    Rules = [fun can_be_blank/1, 
	     parenthesis(fun is_open_bracket/1, 
                         fun is_close_bracket/1), 
	     until(fun is_dollar/1)],
    P = and_parser(Rules),
    case P(T) of
 	{ok, [Key, ""], LinesParsed, Rest} ->
	    gettext_strings(Rest, [{Key, L}|Parsed], L+LinesParsed);
 	{ok, [_Key, Whatever], _LinesParsed, _Rest} ->
 	    {error, {gettext, {Whatever, not_allowed_here}}};
 	{error, Reason} -> 
 	    {error, {gettext, Reason}}
    end;
gettext_strings([H|T], Parsed, L) when [H] == "\r" andalso hd(T) == "\n" ->
    gettext_strings(tl(T), Parsed, L+1);
gettext_strings([H|T], Parsed, L) when [H] == "\r" orelse [H] == "\n" ->
    gettext_strings(T, Parsed, L+1);
gettext_strings([_H|T], Parsed, L) ->
    gettext_strings(T, Parsed, L).

   
%%====================================================================
%% Internal functions
%%====================================================================     
%%--------------------------------------------------------------------
%% @spec collect_ift(T::template()) -> if_token()
%%
%% @doc collect if token untill $end if$ is found
%% @end
%%--------------------------------------------------------------------
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
collect_ift([H|Rest], Token, T, Line) when [H] == "\r" andalso hd(Rest) == "\n" ->
    collect_ift(tl(Rest), ["\r\n"|Token], T, Line+1);
collect_ift([H|Rest], Token, T, Line) when [H] == "\r" orelse [H] == "\n" ->
    collect_ift(Rest, [H|Token], T, Line+1);
collect_ift([H|Rest], Token, T, Line) ->
    collect_ift(Rest, [H|Token], T, Line).

%%--------------------------------------------------------------------
%% @spec parse_ift({Test, Then, Else}) -> if_token()
%%
%% @doc if token parser
%% @end
%%--------------------------------------------------------------------
parse_ift({Test, Then, Else}) ->
    case {parse(Then), parse(Else)} of
	{{error, Reason1}, _} -> {error, Reason1};
	{_, {error, Reason2}} -> {error, Reason2};
	{{ok, CThen}, {ok, CElse}} -> 
            TestTok = [list_to_atom(T) || 
                          T <- string:tokens(string:strip(Test), ".")],
	    {ift, 
	     {{attribute, TestTok}, 
	      CThen, CElse}
	    }
    end;
%%--------------------------------------------------------------------
%% @spec parse_ift({Test, Then}) -> if_token()
%%
%% @doc if token parser
%% @end
%%--------------------------------------------------------------------
parse_ift({Test, Then}) ->
    case parse(Then) of
	{error, Reason} -> {error, Reason};
	{ok, CThen} ->
	    {ift, {{attribute, list_to_atom(string:strip(Test))}, CThen}}
    end.

%%--------------------------------------------------------------------
%% @spec and_parser(Rules::rules()) -> parsed()|{error, Reason}
%%
%% @type rules() = [rule()]
%%       rule()  = function(template()).
%% @type parsed() = {ok, token(), Line::int(), Remaining::template()}
%%
%% @doc and_parser of Rules. 
%% Applies each Rule in sequence to the Template passed. 
%% If a rule fails returns an error.
%% @end
%%--------------------------------------------------------------------
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

%%
%% Parser Rules
%%
%%--------------------------------------------------------------------
%% @spec simple(template()) -> parsed()|{error, Reason}
%%
%% @doc output whatever it gets in input. 
%% @end
%%--------------------------------------------------------------------
simple([H|T]) ->
    {ok, H, 0, T}.

%%--------------------------------------------------------------------
%% @spec can_be_blank(template()) -> parsed()|{error, Reason}
%%
%% @doc Check for blank characters at the beginning of template()
%% and removes them if found.
%% @end
%%--------------------------------------------------------------------
can_be_blank([H|T]) ->
    case is_blank(H) of
	true ->
	    strip_blank1(T, 0);
	_ ->
	    {ok, [H|T], 0}
    end.

%%--------------------------------------------------------------------
%% @spec strip_blank(template()) -> parsed()|{error, Reason}
%%
%% @doc removes blank characters at the beginning of template().
%% If no blank is found returns an error.
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @spec until(predicate()) -> parsed()|{error, Reason}
%%
%% @type predicate() = function(template()).
%%
%% @doc until predicate P: 
%% output what it gets until P(H) is true stripping white spaces.
%% @end
%%--------------------------------------------------------------------
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
            Parsed1 = lists:reverse(Parsed),
            TokL = [list_to_atom(X) || X <- string:tokens(Parsed1, ".")],
	    {ok, TokL, Line, T};
	_ ->
	    until(P, T, Line, [H|Parsed])
    end.

%%--------------------------------------------------------------------
%% @spec until_space(predicate()) -> parsed()|{error, Reason}
%%
%% @type predicate() = funfunction(template()).
%%
%% @doc until predicate P: output whatever it gets 
%% until P(H) is true without stripping white spaces.
%% @end
%%--------------------------------------------------------------------
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
            Parsed1 = lists:reverse(Parsed),
            TokL = [list_to_atom(X) || X <- string:tokens(Parsed1, ".")],
	    {ok, TokL, Line, T};
	_ ->
	    until_space(P, T, Line, [H|Parsed])
    end.


%%--------------------------------------------------------------------
%% @spec until_greedy(predicate()) -> parsed()|{error, Reason}
%%
%% @type predicate() = function(template()).
%%
%% @doc until Greedy version: 
%% output whatever it gets until a $ is reached 
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @spec parenthesis(Start::predicate(), 
%%                   Stop::predicate()) -> parsed()|{error, Reason}
%%
%% @doc Match parenthesis: 
%% Start and Stop are two predicates which matches open and 
%% closed parenthesis. Inner parenthesis are collected to be parsed 
%% later.
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @spec match_char(char(), [char()]) -> bool()
%%
%% @doc Match character.
%% @end
%%--------------------------------------------------------------------
match_char(Char, Val) ->
    [Char] == Val.

%%--------------------------------------------------------------------
%% @spec is_blank(char()) -> bool()
%%
%% @doc Match blank characters.
%% @end
%%--------------------------------------------------------------------
is_blank(C) ->
    match_char(C, " ") 
	orelse match_char(C, "\n")
	orelse match_char(C, "\r").

%%--------------------------------------------------------------------
%% @spec is_dollar(char()) -> bool()
%%
%% @doc Match $ character.
%% @end
%%--------------------------------------------------------------------
is_dollar(C) ->
    match_char(C, "$").

%%--------------------------------------------------------------------
%% @spec is_open_bracket(char()) -> bool()
%%
%% @doc Match { character.
%% @end
%%--------------------------------------------------------------------
is_open_bracket(C) ->
    match_char(C, "{").

%%--------------------------------------------------------------------
%% @spec is_close_bracket(char()) -> bool()
%%
%% @doc Match } character.
%% @end
%%--------------------------------------------------------------------
is_close_bracket(C) ->
    match_char(C, "}").
