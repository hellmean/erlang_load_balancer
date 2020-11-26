-module(my_debug_provider_with_stack).

-behaviour(provider).

-export([start/2, handle_check/1]).

handle_check([]) ->
    {true, []};
handle_check([Health | Rest]) ->
    {Health, Rest}.

start(Id, HealthStack) ->
    provider:start_link(?MODULE, Id, HealthStack).
