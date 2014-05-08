-module({{name}}_tests).

-include_lib("eunit/include/eunit.hrl").

{{name}}_test_() ->
    {setup,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [
      {"{{name}} is alive",
       fun() ->
               %% format is always: expected, actual
               ?assertEqual(howdy, {{name}}:hello())
       end}
      ]}.

