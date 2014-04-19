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
      {with, [fun ?MODULE:lookup/1]}
     ]}}.


setup() ->
    %% Uncomment to debug tests
    %% dbg:tracer(),
    %% dbg:p(all, call),
    %% dbg:tpl(tg_store, []),
    %% dbg:tpl(tg_store_test, []),
    {ok, Store} = tg_store:new(),
    Store.

teardown(_Store) ->
    ok.

%%

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
