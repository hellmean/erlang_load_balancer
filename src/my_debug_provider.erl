-module(my_debug_provider).

-behaviour(provider).

-export([start/1, handle_check/1]).

handle_check({Health}) ->
    {not Health, {not Health}}.

start(Id) ->
    provider:start_link(?MODULE, Id, {false}).
