%%%-------------------------------------------------------------------
%% @doc erlang_load_balancer public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_load_balancer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_load_balancer_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
