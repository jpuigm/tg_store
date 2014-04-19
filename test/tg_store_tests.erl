-module(tg_store_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

tg_store_test_() ->
    {"Tests associative store",
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      {with, [fun ?MODULE:new/1]},
      {with, [fun ?MODULE:new_returns_empty_store/1]}
     ]}}.


setup() ->
    {ok, Store} = tg_store:new(),
    Store.

teardown(_Store) ->
    ok.

%%

new(_) ->
    {"Creates a new store", ?assertMatch({ok, _}, tg_store:new())}.

new_returns_empty_store(S) ->
    {"A new store is created empty", ?assertEqual(0, tg_store:size(S))}.
