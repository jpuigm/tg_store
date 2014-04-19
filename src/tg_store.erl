%%%-------------------------------------------------------------------
%%% @author Juan Puig Martinez <juan.puig@gmail.com>
%%% @copyright (C) 2014, Juan Puig Martinez
%%% @doc
%%% Associative store in which values are associated with tags.
%%% It is possible to store a tag/value pair, and to look up the value(s)
%%% associated with a tag.
%%% @end
%%% Created : 15 Apr 2014 by Juan Puig Martinez <juan.puig@gmail.com>
%%%-------------------------------------------------------------------
-module(tg_store).

-export([new/0]).
-export([size/1]).
-export([add/3]).
-export([lookup/2]).

-type empty_store() :: [].
-type store() :: empty_store() | list({atom(), any()}).

-spec new() -> {ok, empty_store()}.
new() ->
    {ok, []}.

-spec size(store()) -> non_neg_integer().
size(Store) ->
    length(Store).

-spec add(atom(), any(), store()) -> {ok, store()}.
add(Tag, Value, Store) ->
    {ok, [{Tag, Value} | Store]}.

-spec lookup(atom(), store()) -> {ok, any()} | {error, not_found}.
lookup(Tag, Store) ->
    case proplists:get_value(Tag, Store) of
        undefined ->
            {error, not_found};
        Value ->
            {ok, Value}
    end.

