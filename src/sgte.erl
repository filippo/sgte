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
%%% Created : 13 Sep 2006 by filippo pacini <pacini@sgconsulting.it>
%%%-------------------------------------------------------------------
-module(sgte).

-ifdef(ERLHIVE).
-import(.file).         % erlhive uses package notation
-endif.

%% API
-export([compile/1, 
         compile_file/1, 
         render/2, 
         render/3, 
         render_str/2,
         render_str/3,
         render_bin/2,
         render_bin/3,
         gettext_strings/1,
         gettext_init/1,
         gettext_init/2,
         gettext_init/3,
         rec_to_name_kv/2,
         rec_to_kv/2]).


%%yaws_tei is not in a public release yet -behaviour(yaws_tei).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec compile(T::template()) -> {ok, C::compiled()} | {error,Reason}
%%
%%   @type template() = string() | binary(). Template to compile (string or binary)
%%   @type compiled() = [char()|token()]
%%          token() = tupe().
%%
%% @doc Compiles the template string T and returns the compiled 
%% template or an error.
%% @end
%%--------------------------------------------------------------------
compile(T) when is_binary(T) ->
    sgte_parse:parse(binary_to_list(T));
compile(T) when is_list(T) ->
    sgte_parse:parse(T).

%%--------------------------------------------------------------------
%% @spec compile_file(FileName) -> {ok, C::compiled()} | {error,Reason}
%%
%% @doc Compiles the template file FileName and returns the compiled 
%% template or an error.
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
%% @spec render(C::compiled(), 
%%              Data::data(), 
%%              Options::options()) -> string()
%%
%% @type data() = [tuple()]|dict()
%% @type options() = [option()]
%%       option()  = quiet|{gettext_lc, string()}.
%%
%% @doc Renders the compiled template.
%% @end
%%--------------------------------------------------------------------
render(Compiled, Data, Options) ->
    sgte_render:render(Compiled, Data, Options).

%%--------------------------------------------------------------------
%% @spec render(Compiled::compiled(), Data::data()) -> string()
%%
%% @doc Renders the compiled template.
%% @end
%%--------------------------------------------------------------------
render(Compiled, Data) ->
    sgte_render:render(Compiled, Data).

%%--------------------------------------------------------------------
%% @spec render_str(Compiled::compiled(), Data::data(), Options::options()) -> string()
%%
%% @doc Renders the template converting the result to string.
%% @end
%%--------------------------------------------------------------------
render_str(Compiled, Data, Options) ->
    sgte_render:render_str(Compiled, Data, Options).

%%--------------------------------------------------------------------
%% @spec render_str(compiled(), data()) -> string()
%% @doc Calls render/2 and converts the result to string.
%% @end
%%--------------------------------------------------------------------
render_str(Compiled, Data) ->
    sgte_render:render_str(Compiled, Data).

%%--------------------------------------------------------------------
%% @spec render_bin(compiled(), data(), options()) -> binary()
%% @doc Calls render/3 and converts the result to binary.
%% @end
%%--------------------------------------------------------------------
render_bin(Compiled, Data, Options) ->
    sgte_render:render_bin(Compiled, Data, Options).

%%--------------------------------------------------------------------
%% @spec render_bin(compiled(), data()) -> binary()
%% @doc Calls render/2 and converts the result to binary.
%% @end
%%--------------------------------------------------------------------
render_bin(Compiled, Data) ->
    sgte_render:render_bin(Compiled, Data).

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
gettext_strings(Template) when is_binary(Template) ->
    sgte_parse:gettext_strings(binary_to_list(Template));
gettext_strings(FileName) ->
    case file:read_file(FileName) of
	{ok, Bin} ->
	    gettext_strings(Bin);
	Err -> 
	    Err
    end.

%%--------------------------------------------------------------------
%% @spec gettext_init(SrcFiles::src_files()) ->
%%                            ok | {error, Reason}
%%
%% @type src_files()  = [string()]. Source files to parse for gettext strings.
%%
%% @doc Creates the gettext template file (.pot). 
%% SrcFiles is the list of files to be parsed for gettext strings. 
%% Each gettext string found will be written to the .pot file. 
%% The default name of the generated file will be messages.pot.
%% @end
%%--------------------------------------------------------------------
gettext_init(SrcFiles) ->
    {ok, TargetDir} = file:get_cwd(),
    sgte_gettext:gettext_init(TargetDir, SrcFiles, "messages.pot").

%%--------------------------------------------------------------------
%% @spec gettext_init(TargetDir::target_dir(), 
%%                    SrcFiles::src_files()) -> 
%%                            ok | {error, Reason}
%%
%% @type target_dir() = string(). Dir where the .pot file will be written
%%
%% @doc Creates the gettext template file (.pot). 
%% TargetDir is the directory where the file will be created. 
%% If TargetDir doesn't exists it will be created. 
%% SrcFiles is the list of files to be parsed for gettext strings. 
%% Each gettext string found will be written to the .pot file. 
%% The file name of the generated file will be messages.pot.
%% @end
%%--------------------------------------------------------------------
gettext_init(TargetDir, SrcFiles) ->
    sgte_gettext:gettext_init(TargetDir, SrcFiles, "messages.pot").

%%--------------------------------------------------------------------
%% @spec gettext_init(TargetDir::target_dir(), 
%%                    SrcFiles::src_files(), 
%%                    Domain::domain()) -> 
%%                            ok | {error, Reason}
%%
%% @type domain() = string(). The name of the .po file to write.
%%
%% @doc Creates the gettext template file (.pot). 
%% TargetDir is the directory where the file will be created. 
%% If TargetDir doesn't exists it will be created. 
%% SrcFiles is the list of files to be parsed for gettext strings. 
%% Each gettext string found will be written to the .pot file. 
%% Domain (when present) is the name of the file to generate. 
%% If no Domain is defined the default name will be messages.pot.
%% @end
%%--------------------------------------------------------------------
gettext_init(TargetDir, SrcFiles, Domain) ->
    sgte_gettext:gettext_init(TargetDir, SrcFiles, Domain).

%%--------------------------------------------------------------------
%% @spec rec_to_name_kv(RecordTuple, Keys::[atom()]) -> sgte_record() |
%%                                         {error, not_enough_keys} |
%%                                         {error, too_much_keys}
%% @doc TODO: write doc
%% @end
%%--------------------------------------------------------------------
rec_to_name_kv(RecordTuple, Keys) ->
    sgte_dict:rec_to_name_kv(RecordTuple, Keys).


%%--------------------------------------------------------------------
%% @spec rec_to_kv(RecordTuple, Keys::[atom()]) -> sgte_record() |
%%                                         {error, not_enough_keys} |
%%                                         {error, too_much_keys}
%% @doc TODO: write doc
%% @end
%%--------------------------------------------------------------------
rec_to_kv(RecordTuple, Keys) ->
    sgte_dict:rec_to_kv(RecordTuple, Keys).


