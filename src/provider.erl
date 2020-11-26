-module(provider).

-export([start_link/3, loop/3, call/2, cast/2, check/1, check_cast/1, get/1]).

-callback handle_check(State :: term()) -> {IsHealthy :: boolean(), NewState :: term()}.

loop(State, Id, Mod) ->
    receive
        check_cast ->
            {Result, NewState} = Mod:handle_check(State),
            load_balancer:report_back_health(self(), Result),
            loop(NewState, Id, Mod);
        {From, check_call} ->
            {Result, NewState} = Mod:handle_check(State),
            From ! {self(), Result},
            loop(NewState, Id, Mod);
        {From, get_call} ->
            From ! {self(), Id},
            loop(State, Id, Mod)
    end.

start_link(Mod, Id, InitState) ->
    spawn(?MODULE, loop, [InitState, Id, Mod]).

cast(ServerName, Request) ->
    ServerName ! Request.

call(ServerName, Request) ->
    ServerName ! {self(), Request},
    receive
        {ServerName, Reply} ->
            Reply
    end.

check(ServerName) ->
    call(ServerName, check_call).

check_cast(ServerName) ->
    cast(ServerName, check_cast).

get(ServerName) ->
    call(ServerName, get_call).
