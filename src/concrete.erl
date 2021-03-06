%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% vim: set ts=4 sts=4 sw=4 et:
%%
%% @author Seth Falcon
%% @copyright 2013-2014 CHEF Software, Inc.
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

main(["init", Dir]) ->
    concrete_init(Dir);
main(["update"]) ->
    concrete_update();
main(_Args) ->
    io:format("Sorry, you'll have to call me like one of the examples below:\n"
              "concrete init DIR\n"
              "concrete update\n"),
    halt(1).

concrete_init(Dir) ->
    create_directory(Dir),
    io:format("Creating the ~s project with concrete\n\n", [Dir]),
    Name = strip(Dir),
    ActiveApp = yes_no(io:get_line("Would you like an active application? (y/n): ")),
    render_project(Name, ActiveApp),
    render_active(Name, ActiveApp),
    io:format("Now try: cd ~s; make\n", [Dir]),
    ok.

render_project(Name, ActiveApp) ->
    Cmd = [rebar_exe(),
           "create",
           "template_dir=" ++ template_dir(),
           "template=concrete_project",
           "name=" ++ Name,
           "is_active=" ++ atom_to_list(ActiveApp)],
    handle_cmd(run_cmd(Cmd, Name)).
    
render_active(Name, true) ->
    Cmd = [rebar_exe(),
           "create",
           "--force",
           "template_dir=" ++ template_dir(),
           "template=concrete_app",
           "name=" ++ Name],
    handle_cmd(run_cmd(Cmd, Name));
render_active(_Name, _) ->
    ok.

rebar_exe() ->
    filename:join(concrete_dir(), "rebar").

create_directory(Dir) ->
    case filelib:is_file(Dir) of
        false ->
            filelib:ensure_dir(filename:join(Dir, "stub"));
        true ->
            io:format("ERROR: concrete init wants to create '~s', but it already exists\n",
                      [Dir]),
            halt(1)
    end.

strip(S) ->
    string:strip(string:strip(S, both, $\n)).

%% This is duplicated from priv/concrete_project.template, but not
%% sure it's worth DRY'ing up at this point.
-define(UPDATE_FILE_MAP,
        [{"concrete_project_Makefile", "Makefile"},
         {"concrete_project_rebar.config.script", "rebar.config.script"},
         {"concrete_project_concrete.mk", "concrete.mk"}]).

%% Move rebar.config.script -> rebar.config.script.0
%% Move concrete.mk -> concrete.mk.0
%% Put new copies in place
concrete_update() ->
    FilesToBackup = [ Path || {_, Path} <- ?UPDATE_FILE_MAP ],
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
    RealSource = filename:join([template_dir(), Source]),
    case file:copy(RealSource, Dest) of
        {ok, _} ->
            io:format("updated: ~s\n", [Dest]);
        {error, Why} ->
            io:format("Unable to update via copy ~s ~s~n"
                      "error: ~p~n", [RealSource, Dest, Why]),
            halt(1)
    end.

template_dir() ->
    filename:join([concrete_dir(), "priv", "templates"]).

concrete_dir() ->
    %% Note that code:which and code:priv_dir return odd looking
    %% results when run via an escriptized.
    filename:absname(filename:dirname(escript:script_name())).

yes_no(S) ->
    case hd(string:to_lower(strip(S))) of
        $y ->
            true;
        _ ->
            false
    end.

run_cmd(CmdList, Dir) ->
    Cmd = string:join(CmdList, " "),
    Port = erlang:open_port({spawn, Cmd},
                            [{line, 256}, exit_status, stderr_to_stdout,
                             {cd, Dir}]),
    {gather_data(Port, 10000, []), Cmd}.

gather_data(Port, Timeout, Acc) ->
    receive
        {Port, {exit_status, Status}} ->
            {Status, erlang:iolist_to_binary(lists:reverse(Acc))};
        {Port, {data, {eol, Line}}} ->
            gather_data(Port, Timeout, ["\n", Line | Acc])
    after Timeout ->
            timeout
    end.

handle_cmd({{0, _}, _}) ->
    ok;
handle_cmd({{Status, Out}, Cmd}) ->
    io:format("ERROR: command failed with status ~p.\ncommand: ~s\noutput:\n~s\n\n",
              [Status, Cmd, Out]),
    halt(1).
