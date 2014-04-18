-module(tg_store_tests).

-include_lib("eunit/include/eunit.hrl").

tg_store_test_() ->
    {setup,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [
      {"tg_store creation", fun() -> ?assertEqual({ok, []}, tg_store:new()) end}
      ]}.
