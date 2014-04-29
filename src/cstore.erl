%%%-------------------------------------------------------------------
%%% @author Juan Puig Martinez <juan.puig@gmail.com>
%%% @copyright (C) 2014, Juan Puig Martinez
%%% @doc
%%% Interface with a Distributed Associative Store.
%%% @end
%%% Created : 19 Apr 2014 by Juan Puig Martinez <juan.puig@gmail.com>
%%%-------------------------------------------------------------------
-module(cstore).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([new/1,new/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {nodes}).

%%%===================================================================
%%% API
%%%===================================================================

new(Nodename) ->
    new('localhost', Nodename).

new(Host, Nodename) ->
    gen_server:call(?SERVER, {new, Host, Nodename}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{nodes = []}}.

handle_call({new, Host, Nodename}, _From, State) ->
    Reply = {ok, Node} = slave:start(Host, Nodename),
    NewState = State#state{nodes = [Node | State#state.nodes]},
    {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
