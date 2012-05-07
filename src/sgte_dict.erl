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
         rstore/3,
         rfoldl/2,
         rfoldr/2,
         rappend/3,
         find/2,
         store/3,
         merge/3,
         from_list/1,
         rec_to_name_kv/2,
         rec_to_kv/2]).

%% recursive find
rfind(K, L) when not is_list(K) ->
    ?MODULE:rfind([K], L);
rfind([], L) ->
    {ok, L};
rfind([K|_]=F, L) when is_list(L) ->
    rfind_proplist(proplists:get_value(K, L), F);
rfind([K|_]=F, L) when element(1, L) =:= dict ->
    rfind_dict(dict:find(K, L), F);
rfind(_, _) ->
    {error, invalid_dict}.

rfind_dict(error, [H|_]) ->
    {error, H};
rfind_dict({ok, D}, [_|T]) ->
    ?MODULE:rfind(T, D).

rfind_proplist(undefined, [H|_]) ->
    {error, H};
rfind_proplist(V, [_|T]) ->
    ?MODULE:rfind(T, V).

rstore(K, V, L) when not is_list(K) ->
    ?MODULE:rstore([K], V, L);
rstore([Key], V, D) when element(1, D) =:= dict ->
    dict:store(Key, V, D);
rstore([Key|T], V, D) when element(1, D) =:= dict ->
    D1 = find(Key, D, fun dict:new/1),
    dict:store(Key, ?MODULE:rstore(T, V, D1), D);
rstore([Key], V, L) when is_list(L) ->
    L1 = proplists:delete(Key, L),
    [{Key, V}|L1];
rstore([Key|T], V, L) when is_list(L) ->
    SL = proplists:get_value(Key, L, []),
    L1 = proplists:delete(Key, L),
    [{Key, ?MODULE:rstore(T, V, SL)}|L1];
rstore(_, _, _) ->
    {error, invalid_dict}.

rfoldacc({K, V}, Acc) ->
    rstore(K, V, Acc).

rfoldl(KV, L) ->
    lists:foldl(fun rfoldacc/2, L, KV).

rfoldr(KV, L) ->
    lists:foldr(fun rfoldacc/2, L, KV).

rappend(K, V, L) ->
    case rfind(K, L) of
        {error, _} ->
            rstore(K, [V], L);
        {ok, A} ->
            V1 = lists:reverse([V|lists:reverse(A)]),
            rstore(K, V1, L)
    end.

find(Key, L, Default) when is_list(L), is_function(Default) ->
    find1(find(Key, L), Default);
find(Key, L, Default) when is_list(L) ->
    proplists:get_value(Key, L, Default);
find(Key, Dict, Default) ->
    find1(dict:find(Key, Dict), Default).

find1(A, Default) when A == error; A == undefined, is_function(Default) ->
    Default();
find1(A, Default) when A == error; A == undefined ->
    Default;
find1({ok, V}, _) ->
    V;
find1(V, _) ->
    V.

find(Key, L) when is_list(L) ->
    find1(proplists:get_value(Key, L));
find(Key, Dict) ->
    dict:find(Key, Dict).

find1(A) when A == none; A == undefined; A == error ->
    error;
find1(V) ->
    {ok, V}.

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
