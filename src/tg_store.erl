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

-spec new() -> {ok, []}.
new() ->
    {ok, []}.

-spec size(list()) -> non_neg_integer().
size(Store) ->
    length(Store).



