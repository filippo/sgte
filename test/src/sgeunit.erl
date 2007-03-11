%%%-------------------------------------------------------------------
%%% File    : sgeunit.erl
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
%%% 2006 S.G. Consulting srl. All Rights Reserved.
%%%
%%% Description : 
%%%
%%% Created :  4 Sep 2006 by filippo pacini <pacini@sgconsulting.it>
%%%-------------------------------------------------------------------
-module(sgeunit).

-author("$Author: filippo $").
-version("$Revision: 1.12 $").
-date("$Date: 2006/10/08 13:49:39 $").

-export([run/1, run/2]).
-export([assert_equal/1, assert_equal/2, assert_not_equal/2]).
-export([assert_raises/2, assert_raises/3, fail/0]).

%% sgeunit test
-export([test_ok/0, test_fail/0, test_not_equal/0, test_equal_list/0, test_multiple/0, test_raises/0]).


%%--------------------
%%
%% Module API
%%
%%--------------------
%% Get list of exported functions from Module.
%% Execute setup_test if exported from Module
%% run all test_* in Module collecting results.
%% Execute end_test if exported from Module
run(Module) ->
    ExportList = Module:module_info(exports),
    setup(Module, ExportList),
    Result = collect(run_tests(Module, ExportList, [], false)),
    end_t(Module, ExportList),
    Result.
%% Same as above, but report results verbosely (i.e. prints also succesfull tests) 
run(Module, {verbose}) ->
    ExportList = Module:module_info(exports),
    setup(Module, ExportList),
    Result = collect(run_tests(Module, ExportList, [], verbose)),
    end_t(Module, ExportList),
    Result.



%% Assert Val1 == Val2
assert_equal(Val1, Val2) ->
    case Val1 == Val2 of
        true ->
            ok;
        _ ->
            {error, {expected, Val1, got, Val2}}
    end.

assert_equal(ValueList) ->
    Test = lists:nth(1, ValueList),
    case lists:all(fun (El) -> El == Test end, ValueList) of
        true ->
            ok;
        _ ->
            {error, {expected_all, Test, got, ValueList}}
    end.

%% Assert Val1 != Val2
assert_not_equal(Val1, Val2) ->
    case Val1 == Val2 of
        true ->
            {error, {expected, Val1, not_equal_to, Val2}};
        _ ->
	    ok
    end.

assert_raises(Fun, Error) ->
    Result = (catch Fun()),
    case Result of
        Error ->
            ok;
        _ ->
            {error, "expected ~p got: ~p", [Error, Result]}
    end.
assert_raises(Fun, Args, Error) ->
    Result = (catch Fun(Args)),
    case Result of
        Error ->
            ok;
        _ ->
            {error, "expected ~p got: ~p", [Error, Result]}
    end.

%% Fail test
fail() ->
    {error, {failed}}.


%%--------------------
%%
%% Internal functions
%%
%%--------------------
collect(ResultList) ->
    case lists:all(fun (El) -> El == ok end, ResultList) of
	true ->
	    ok;
	false ->
	    error
    end.    

%% Test setup
setup(Mod, FunList) ->
    case lists:keysearch(setup_test, 1, FunList) of
	{value, _} ->
	    Mod:setup_test();
	_ ->
	    done
    end.

%% Test finalization
end_t(Mod, FunList) ->
    case lists:keysearch(end_test, 1, FunList) of
	{value, _} ->
	    Mod:end_test();
	_ ->
	    done
    end.

%%
%% run all tests in module
%%
run_tests(_, [], ResultList, _Verbose) ->
    ResultList;
run_tests(Module, [H|T], ResultList, Verbose) ->
    FName = element(1, H),
    %%io:format("~p:~p~n", [Module, FName]),
    case lists:sublist(atom_to_list(FName), 5) of
        "test_" ->
	    Next = report_result(Module, FName, Module:FName(), Verbose);
	_ ->
            Next = ok
    end,
    run_tests(Module, T, [Next|ResultList], Verbose).

%%
%% report test results
%%
report_result(Module, FName, Result, Verbose) when is_list(Result) == true ->
    F = report_closure(Module, FName, Verbose),
    collect([F(X) || X <- Result]);
report_result(Module, FName, Result, Verbose) ->
    F = report_closure(Module, FName, Verbose),
    F(Result).
    
report_closure(Module, FName, Verbose) ->
    fun (Result) ->
	    case {Result, Verbose} of
		{ok, verbose} ->
		    io:format("[pass]  .... ~p:~p~n", [Module, FName]),
		    ok;
		{ok, _} ->
		    ok;
		{{error, Msg}, _} ->
		    io:format("[ERROR] .... ~p:~p ---> ~p~n", [Module, FName, Msg]),
		    error
	    end
    end.

%%---------------------------------
%%
%% TEST to test the this module
%%
%%---------------------------------
test_ok() ->
    assert_equal(3+5, 8).
test_fail() ->
    assert_equal(3+5, 7).
test_not_equal() ->
    assert_not_equal(3+5, 7).
test_equal_list() ->
    assert_equal([3+5, 8, 7+1, 6+2, 4+4, 5+3]).
test_multiple() ->
    T1 = fun test_ok/0,
    T2 = fun test_fail/0,
    T3 = fun test_not_equal/0,
    [X() || X <- [T1, T2, T3]].
test_raises() ->
    Test = fun() -> throw({error}) end,
    Test2 = fun(_Args) -> throw({error}) end,
    [assert_raises(Test, {error}), assert_raises(Test2, {foo}, {error})].
