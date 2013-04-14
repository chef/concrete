%%% vim: set ts=4 sts=4 sw=4 et:

%% @author Seth Falcon
%% @copyright 2013 Seth Falcon
%% @doc TODO
%%
%%
%% See the README.md file for details on how to use the script in your
%% build and notes on implementation.
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
         'init-makefile'/2
        ]).

'init-makefile'(Config, _AppFile) ->
    run_on_base_dir(Config, fun init_makefile/1).

run_on_base_dir(Config, Fun) ->
    case rebar_utils:processing_base_dir(Config) of
        true -> Fun(Config);
        false -> ok
    end.

init_makefile(_Config) ->
    io:format("Hello from concrete: ~p~n", [code:priv_dir(concrete)]),
    MyDir = code:priv_dir(concrete),
    ok = filelib:ensure_dir(".concrete/ENSURE"),
    CMD = lists:flatten(["cp ", MyDir, "/rebar.mk .concrete"]),
    os:cmd(CMD),
    ok.
    %% DepsDir = rebar_config:get(Config, deps_dir, "deps"),
    %% Ignores = string:tokens(rebar_config:get_global(Config, ignore, ""), ","),
    %% DepDirs = deps_dirs(DepsDir),
    %% SubDirs = rebar_config:get(Config, sub_dirs, []),
    %% DepVersions = get_dep_versions(DepDirs),
    %% AllDeps = collect_deps(["."|DepDirs++SubDirs]),
    %% NewDeps = get_locked_deps(DepVersions, AllDeps, Ignores),
    %% NewConfig = rebar_config:get_global(Config,
    %%     lock_config, "./rebar.config.lock"),
    %% write_rebar_lock("./rebar.config", NewConfig, NewDeps),
    %% io:format("wrote locked rebar config to: ~s~n", [NewConfig]),

