-module(tg_store_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

tg_store_test_() ->
    {"Tests associative store",
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      {with, [fun ?MODULE:new_returns_empty_store/1]},
      {with, [fun ?MODULE:add/1]},
      {with, [fun ?MODULE:lookup/1]},
      {with, [fun ?MODULE:failed_lookup/1]}
     ]}}.


setup() ->
    {ok, Store} = tg_store:new(),
    Store.

teardown(_Store) ->
    ok.

%% tests

new_returns_empty_store(S) ->
    ?assertEqual(0, tg_store:size(S)).

add(S) ->
    Size = tg_store:size(S),
    {ok, NewS} = tg_store:add(a,b,S),
    ?assertEqual(Size + 1, tg_store:size(NewS)).

lookup(S) ->
    {Tag, Value} = {ship, "Apollo 13"},
    {ok, NewS} = tg_store:add(Tag, Value, S),
    ?assertEqual({ok, Value}, tg_store:lookup(Tag, NewS)).

failed_lookup(S) ->
    ?assertEqual({error, not_found}, tg_store:lookup(shark, S)).
