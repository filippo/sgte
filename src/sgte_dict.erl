%%%-------------------------------------------------------------------
%%% File    : sgte_dict
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
%%% <p>Implements a dictionary and a record like data structure. 
%%% Uses the dict module and extends it to support nested dicts.</p>
%%%
%%% <p>This module is the interface used to access data in the 
%%% render phase.</p>
%%% @end
%%%
%%% Created :  3 Sep 2007 by filippo pacini <pacini@sgconsulting.it>
%%%-------------------------------------------------------------------
-module(sgte_dict).

-ifdef(ERLHIVE).
-import(.lists).    % erlhive uses package notation
-import(.dict).     % ditto
-endif.

-export([rfind/2, 
         find/2, 
         store/3, 
         merge/3, 
         from_list/1, 
         rec_to_name_kv/2,
         rec_to_kv/2]).

%% recursive find
rfind([Key], Dict) ->
    case dict:find(Key, Dict) of
        error ->
            {error, Key};
        {ok, V} ->
            {ok, V}
    end;
rfind([H|T], Dict) ->
    case dict:find(H, Dict) of
        error ->
            {error, H};
        {ok, D} when is_list(D) ->
            ?MODULE:rfind(T, dict:from_list(D));
        {ok, D} ->
            ?MODULE:rfind(T, D)
    end.

find(Key, Dict) when is_list(Dict) ->
    dict:find(Key, dict:from_list(Dict));
find(Key, Dict) ->
    dict:find(Key, Dict).

store(Key, Value, Dict) ->
    dict:store(Key, Value, Dict).

merge(Fun, Dict1, Dict2) ->
    dict:merge(Fun, Dict1, Dict2).

from_list(List) ->
    dict:from_list(List).

rec_to_name_kv(RecordTuple, Keys) ->
    [Name|Values] = tuple_to_list(RecordTuple),
    case length(Values) =:= length(Keys) of
        true ->
            {Name, lists:zip(Keys, Values)};
        false ->
            case length(Values) > length(Keys) of
                true ->
                    {error, not_enough_keys};
                _ ->
                    {error, too_much_keys}
            end
    end.
    
rec_to_kv(RecordTuple, Keys) ->
    [_Name|Values] = tuple_to_list(RecordTuple),
    case length(Values) =:= length(Keys) of
        true ->
            lists:zip(Keys, Values);
        false ->
            case length(Values) > length(Keys) of
                true ->
                    {error, not_enough_keys};
                _ ->
                    {error, too_much_keys}
            end
    end.
