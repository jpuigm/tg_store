%%%-------------------------------------------------------------------
%%% @author Juan Puig Martinez <juan.puig@gmail.com>
%%% @copyright (C) 2014, Juan Puig Martinez
%%% @doc
%%% Supervisor of the tg_store_server.
%%% @end
%%% Created : 20 Apr 2014 by Juan Puig Martinez <juan.puig@gmail.com>
%%%-------------------------------------------------------------------
-module(tg_store_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {'tg_store_server', {'tg_store_server', start_link, []},
              Restart, Shutdown, Type, ['tg_store_server']},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
