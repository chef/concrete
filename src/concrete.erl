%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% vim: set ts=4 sts=4 sw=4 et:
%%
%% @author Seth Falcon
%% @copyright 2013 Opscode, Inc.
%%
%% @doc concrete escript module.
%% concrete is intended to enhance rebar based Erlang projects by
%% providing a common Makefile wrapper and an easy way to include
%% development only dependencies. This module implements an escript
%% used to initialize new projects and to update the concrete files in
%% an existing project.
%% @end
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(concrete).
-author("Seth Falcon <seth@opscode.com>").

-export([
         main/1
        ]).

main(["init"]) ->
    concrete_init();
main(["update"]) ->
    concrete_update();
main(_Args) ->
    io:format("Sorry, you'll have to call me like one of the examples below:\n"
              "concrete init\n"
              "concrete update\n"),
    halt(1).

concrete_init() ->
    verify_empty_directory(),
    io:format("Initialize a new project with concrete\n\n"),
    Name = strip(io:get_line("Project name: ")),
    Desc = strip(io:get_line("Short Description:\n")),
    CmdFmt = "rebar create template=concrete_project name=~s description=\"~s\"",
    Cmd = io_lib:format(CmdFmt, [Name, Desc]),
    io:format("Creating ~s via '~s'~n", [Name, Cmd]),
    os:cmd(Cmd),
    io:format("Now try: make\n"),
    ok.

verify_empty_directory() ->
    case filelib:wildcard("*") of
        [] ->
            ok;
        Files ->
            io:format("ERROR: concrete init wants an empty directory\n"
                      "       But we found: ~p\n", [Files]),
            halt(1)
    end.

strip(S) ->
    string:strip(string:strip(S, both, $\n)).

%% This is duplicated from priv/concrete_project.template, but not
%% sure it's worth DRY'ing up at this point.
-define(UPDATE_FILE_MAP,
        [{"concrete_project_Makefile", "Makefile"},
         {"concrete_project_rebar.config.script", "rebar.config.script"},
         {"concrete_project_rebar.mk", ".concrete/rebar.mk"}]).

%% Move rebar.config.script -> rebar.config.script.0
%% Move .concrete/rebar.mk -> .concrete/rebar.mk.0
%% Put new copies in place
concrete_update() ->
    FilesToBackup = [ Path || {_, Path} <- ?UPDATE_FILE_MAP ],
    filelib:ensure_dir(".concrete/ENSURE"),
    make_backups(FilesToBackup),
    make_copies(?UPDATE_FILE_MAP),
    ok.

make_backups(Paths) ->
    [ make_backup(Path) || Path <- Paths ].

make_backup(Path) ->
    case filelib:is_file(Path) of
        true ->
            case file:copy(Path, Path ++ ".0") of
                {ok, _} ->
                    io:format("backup: ~s -> ~s~n",
                              [Path, Path ++ ".0"]),
                    ok;
                {error, Why} ->
                    io:format("Unable to create a backup copy of '~s'\n"
                              "error: ~p~n", [Path, Why]),
                    halt(1)
            end;
        false ->
            ok
    end.

make_copies(Map) ->
    [ make_copy(Source, Dest) || {Source, Dest} <- Map ].

make_copy(Source, Dest) ->
    %% Note that code:which and code:priv_dir return odd looking
    %% results when run via an escriptized.
    ConcreteDir = filename:dirname(escript:script_name()),
    RealSource = filename:join([ConcreteDir, "priv", "templates", Source]),
    case file:copy(RealSource, Dest) of
        {ok, _} ->
            io:format("updated: ~s\n", [Dest]);
        {error, Why} ->
            io:format("Unable to update via copy ~s ~s~n"
                      "error: ~p~n", [RealSource, Dest, Why]),
            halt(1)
    end.
