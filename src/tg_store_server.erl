%%%-------------------------------------------------------------------
%%% @author Juan Puig Martinez <juan.puig@gmail.com>
%%% @copyright (C) 2014, Juan Puig Martinez
%%% @doc
%%% Server that maintains an associative store.
%%% @end
%%% Created : 19 Apr 2014 by Juan Puig Martinez <juan.puig@gmail.com>
%%%-------------------------------------------------------------------
-module(tg_store_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add/2]).
-export([add/3]).
-export([lookup/1]).
-export([size/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(REMOTE_CALL_TIMEOUT, 1000).

-record(state, {store}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add(atom(), any()) -> ok.
add(Tag, Value) ->
    gen_server:call(?SERVER, {add, Tag, Value}).

-spec add(atom(), any(), list(node())) -> ok | {timeout, list(node())}.
add(Tag, Value, Nodes) ->
    add(Tag, Value, Nodes, []).

-spec lookup(atom()) -> {ok, any()} | {error, not_found}.
lookup(Tag) ->
    gen_server:call(?SERVER, {lookup, Tag}).

-spec size() -> non_neg_integer().
size() ->
    gen_server:call(?SERVER, size).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, Store} = tg_store:new(),
    {ok, #state{store = Store}}.


handle_call({add, Tag, Value}, _From, State = #state{store = Store}) ->
    {ok, NewStore} = tg_store:add(Tag, Value, Store),
    lager:log(info, ?MODULE, "Added {~p,~p} to the store", [Tag, Value]),
    {reply, ok, State#state{store = NewStore}};
handle_call({lookup, Tag}, _From, State = #state{store = Store}) ->
    Reply = tg_store:lookup(Tag, Store),
    lager:log(info, ?MODULE, "Lookup of key: ~p with result: ~p", [Tag, Reply]),
    {reply, Reply, State};
handle_call(size, _From, State = #state{store = Store}) ->
    Reply = tg_store:size(Store),
    {reply, Reply, State}.


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

%% add gen_server:multi_call/4?
add(_Tag, _Value, [], []) ->
    ok;
add(_Tag, _Value, [], Failed) ->
    lager:log(info, ?MODULE, "Failed nodes to add values: ~p", [Failed]),
    {timeout, Failed};
add(Tag, Value, [Node | Nodes], Failed) when Node =:= node() ->
    ok = add(Tag, Value),
    add(Tag, Value, Nodes, Failed);
add(Tag, Value, [Node | Nodes], Failed) ->
    case gen_server:call({node, Node}, {add, Tag, Value}, ?REMOTE_CALL_TIMEOUT) of
        ok ->
            lager:log(info, ?MODULE, "Successful remote add on node ~p", [Node]),
            add(Tag, Value, Nodes, Failed);
        _ ->
            lager:log(warning, ?MODULE, "Failed remote add on node ~p", [Node]),
            add(Tag, Value, Nodes, [Node | Failed])
    end.
