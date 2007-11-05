%%%-------------------------------------------------------------------
%%% File    : sgte_gettext.erl
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
%%% <p>Helper functions to generate the gettext po template 
%%% file (.pot).</p>
%%%
%%% <p>This module is not meant to be used directly. It's called 
%%% through the interface of the sgte module.</p>
%%% @end
%%%
%%% Created :  7 Aug 2007 by Filippo Pacini <pacini@sgconsulting.it>
%%%-------------------------------------------------------------------
-module(sgte_gettext).

-ifdef(ERLHIVE).
-import(.file).    % erlhive uses package notation
-import(.lists).   % ditto
-endif.

%% API
-export([gettext_init/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec gettext_init(TargetDir::target_dir(), 
%%                    SrcFiles::src_files(), 
%%                    Domain::domain()) -> 
%%                            ok | {error, Reason}
%%
%% @doc Creates the gettext template file (.pot). 
%% TargetDir is the path (terminated with /) where the file will be created. 
%% If TargetDir doesn't exists it will be created. 
%% SrcFiles is the list of files to be parsed for gettext strings. 
%% Each gettext string found will be written to the .pot file. 
%% Domain (when present) is the name of the file to generate. 
%% If no Domain is defined the default name will be messages.pot.
%% @end
%%--------------------------------------------------------------------
gettext_init(TargetDir, SrcFiles, Domain) ->
    %% Create target directory and parent directories
    %% when missing.
    ok = filelib:ensure_dir(TargetDir),
    %% Create the .pot file
    case file:open(filename:join([TargetDir, Domain]), write) of
        {ok, Fd} ->
            %% Write .pot header
            io:format(Fd, po_header(), []),
            %% Write strings from each source file
            [write_file(Fd, FName) || FName <- SrcFiles],
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec write_file(Fd::io_device(), FName::string()) -> 
%%                                                 ok | {error, Reason}
%%
%% @doc Parses FName for gettext_strings and writes them to Fd file.
%% @end
%%--------------------------------------------------------------------
write_file(Fd, FName) ->
    case file:read_file(FName) of
        {ok, Bin} ->
            Keys = sgte_parse:gettext_strings(binary_to_list(Bin)),
            Entries = [gettext_entry(
                         {FName, {Key, LineNo}, Key})
                       || {Key, LineNo} <- Keys],
            [io:format(Fd, E, []) || E <- Entries],
            ok;
        {error, Reason} ->
            throw({error, Reason})
    end.

%%--------------------------------------------------------------------
%% @spec gettext_entry(E::gettext_entry()) -> string()
%%
%% @type gettext_entry() = {FName::string(), gettext_tuple(), Default}
%%
%% @doc Returns a string to be written to the gettext .po file.
%% @end
%%--------------------------------------------------------------------
gettext_entry({FName, {Key, LineNo}, Default}) ->
    {ok, C} = sgte:compile("~n#: $fname$:$line_no$~n"
                           "msgid \"$key$\"~n"
                           "msgstr \"$default$\"~n"),
    Res = sgte:render_str(C, 
                          [{fname, FName}, 
                           {line_no, integer_to_list(LineNo)}, 
                           {key, Key}, 
                           {default, Default}]),
    Res.

%%--------------------------------------------------------------------
%% @spec po_header() -> string()
%%
%% @doc Returns the .pot header file.
%% @end
%%--------------------------------------------------------------------
po_header() ->
    {{Y,Mo,D},{H,Mi,S}} = calendar:local_time(),
    Date = lists:flatten(
             io_lib:fwrite(
               "~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
               [Y,Mo,D,H,Mi,S])),
    Res = 
        "# SOME DESCRIPTIVE TITLE.~n"
        "# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER~n"
        "# This file is distributed under the same license as the PACKAGE package.~n"
        "# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.~n"
        "#~n"
        "#, fuzzy~n"
        "msgid \"\"~n"
        "msgstr \"\"~n"
        "\"Project-Id-Version: PACKAGE VERSION\n\"~n"
        "\"Report-Msgid-Bugs-To: \"~n"
        "\"POT-Creation-Date: " 
        ++ lists:sublist(Date, 1, length(Date)-1) ++ "\"~n"
        "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\"~n"
        "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\"~n"
        "\"Language-Team: LANGUAGE <LL@li.org>\"~n"
        "\"MIME-Version: 1.0\"~n"
        "\"Content-Type: text/plain; charset=CHARSET\"~n"
        "\"Content-Transfer-Encoding: 8bit\"~n",
    Res.
