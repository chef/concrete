-module({{name}}_sup).

-behaviour(supervisor).

-export([
         init/1,
         start_link/0
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Restart = {one_for_one, 10, 10},
    {ok, {Restart, []}}.
