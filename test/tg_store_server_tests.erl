-module(tg_store_server_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, tg_store_server).

tg_store_test_() ->
    {"Tests associative store server",
     {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun ?MODULE:store_is_empty/0,
      fun ?MODULE:add/0,
      fun ?MODULE:lookup/0,
      fun ?MODULE:failed_lookup/0
     ]}}.


setup() ->
    {ok, Server} = tg_store_server:start_link(),
    Server.

teardown(Server) ->
    unlink(Server),
    erlang:exit(Server, killed),
    no_heartbeat().

%% tests

store_is_empty() ->
    ?assertEqual(0, tg_store_server:size()).

add() ->
    Size = tg_store_server:size(),
    ?assertEqual(ok, tg_store_server:add(a,b)),
    ?assertEqual(Size + 1, tg_store_server:size()).

lookup() ->
    {Tag, Value} = {ship, "Apollo 13"},
    ok = tg_store_server:add(Tag, Value),
    ?assertEqual({ok, Value}, tg_store_server:lookup(Tag)).

failed_lookup() ->
    ?assertEqual({error, not_found}, tg_store_server:lookup(trex)).

%% helpers

%% no_heartbeat is needed as teardown/setup transition
%% happens too fast.
no_heartbeat() ->
    case whereis(?SERVER) of
        undefined ->
            ok;
        _Pid ->
            timer:sleep(100),
            no_heartbeat()
    end.
