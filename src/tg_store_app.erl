%%%-------------------------------------------------------------------
%%% @author Juan Puig Martinez <juan.puig@gmail.com>
%%% @copyright (C) 2014, Juan Puig Martinez
%%% @doc
%%%
%%% @end
%%% Created : 20 Apr 2014 by Juan Puig Martinez <juan.puig@gmail.com>
%%%-------------------------------------------------------------------
-module(tg_store_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    case 'tg_store_supervisor':start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
                end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
