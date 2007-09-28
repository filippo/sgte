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
%%% Uses ets tables.</p>
%%%
%%% <p>This module is the interface used to access data in the 
%%% render phase.</p>
%%% @end
%%%
%%% Created :  3 Sep 2007 by filippo pacini <pacini@sgconsulting.it>
%%%-------------------------------------------------------------------
-module(sgte_dict).

-export([parse/1,
         parse/2,
         is_dict/1,
         lookup/2,
         store/2,
         merge/2,
         rec_to_name_kv/2,
         rec_to_kv/2]).

%% recursively parses data and builds ets tables
parse(Data, Options) when is_list(Data) ->
    parse([{options, Options}|Data]);
parse(Data, Options) ->
    Table = parse(Data),
    store({options, Options}, Table),
    Table.

parse(Data) ->
    case is_dict(Data) of
        false -> parse_kv(Data);
        true  -> parse_dict(Data)
    end.

parse_kv(Data) ->
    Ref = io_lib:format("~b~b~b", tuple_to_list(erlang:now())),
    Name = list_to_atom(lists:flatten(Ref)),
    Table = ets:new(Name, [set, public]),
    parse_kv(Data, Table).

parse_kv([], Table) ->
    Table;
parse_kv([{Key, [{_K, _V}|_R]=Value}|Rest], Table) ->
    Tid = parse_kv(Value),
    ets:insert(Table, {Key, {ets_table, Tid}}),
    parse_kv(Rest, Table);
parse_kv([{Key, Value}|Rest], Table) ->
    case is_dict(Value) of
        true ->
            Tid = parse_kv(dict:to_list(Value)),
            ets:insert(Table, {Key, {ets_table, Tid}});
        false ->
            ets:insert(Table, {Key, Value})
    end,
    parse_kv(Rest, Table).

parse_dict(Data) ->
    Ref = io_lib:format("~b~b~b", tuple_to_list(erlang:now())),
    Name = list_to_atom(lists:flatten(Ref)),
    Table = ets:new(Name, [set, public]),
    parse_kv(dict:to_list(Data), Table).

lookup(Key, Tab) when is_atom(Key) ->
    case ets:lookup(Tab, Key) of
        [] ->
            {error, Key};
        [{Key, V}] ->
            {ok, V}
    end;
lookup([Key], Tab) ->
    case ets:lookup(Tab, Key) of
        [] ->
            {error, Key};
        [{Key, V}] ->
            {ok, V}
    end;
lookup([Key|Rest], Tab) ->
    case ets:lookup(Tab, Key) of
        [] ->
            {error, Key};
        [{Key, {ets_table, Tid}}] ->
            lookup(Rest, Tid)
    end.

store({K,V}, Tab) ->    
    ets:insert(Tab, {K,V}),
    Tab.

merge(T1, T2) ->
    case ets:match(T2, '$1') of
        [] ->
            T1;
        Objs ->
            [ets:insert(T1, O) || [O] <- Objs],
            T1
    end.

%% check if a Data is an erlang dict
is_dict(Data) when is_tuple(Data) ->
    case element(1, Data) of
        dict ->
            true;
        _ ->
            false
    end;
is_dict(Data) ->
    false.
    
rec_to_name_kv(RecordTuple, Keys) ->
    [Name|Values] = tuple_to_list(RecordTuple),
    case length(Values) =:= length(Keys) of
        true ->
            KVList = lists:zip(Keys, Values),
            {Name, parse_kv(KVList)};
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
            parse_kv(lists:zip(Keys, Values));
        false ->
            case length(Values) > length(Keys) of
                true ->
                    {error, not_enough_keys};
                _ ->
                    {error, too_much_keys}
            end
    end.
