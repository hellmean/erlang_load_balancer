-module(load_balancer_test).
-include_lib("eunit/include/eunit.hrl").

load_balancer_get_empty_test() ->
    load_balancer:start_link(10),
    ?assertMatch(empty, load_balancer:get()),
    load_balancer:stop().

load_balancer_gets_something_after_register_test() ->
    load_balancer:start_link(10),
    Provider = my_debug_provider:start(42),
    load_balancer:register(Provider),
    ?assertMatch(42, load_balancer:get()),
    load_balancer:stop().

load_balancer_exclude_works_test() ->
    load_balancer:start_link(10),
    Provider = my_debug_provider:start(42),
    load_balancer:register(Provider),
    load_balancer:exclude(Provider),
    ?assertMatch(empty, load_balancer:get()),
    load_balancer:stop().

load_balancer_include_works_test() ->
    load_balancer:start_link(10),
    Provider = my_debug_provider:start(42),
    load_balancer:register(Provider),
    load_balancer:exclude(Provider),
    load_balancer:include(Provider),
    ?assertMatch(42, load_balancer:get()),
    load_balancer:stop().

load_balancer_round_robin_works_test() ->
    load_balancer:start_link(10),
    Ids = [1,2,3,4],
    lists:foreach(fun(X) ->
        P = my_debug_provider:start(X),
        load_balancer:register(P)
    end, Ids),
    Actual = lists:map(fun(_) ->
        load_balancer:get()
    end, Ids ++ Ids),
    ?assertMatch([4,3,2,1,4,3,2,1], Actual),
    load_balancer:stop().

load_balancer_health_check_works_with_alternating_debug_provider_test() ->
    load_balancer:start_link(10),
    Provider = my_debug_provider:start(42),
    load_balancer:register(Provider),
    ?assertMatch(42, load_balancer:get()),
    load_balancer:check_health(),
    load_balancer:get_state(),
    ?assertMatch(42, load_balancer:get()),
    load_balancer:check_health(),
    load_balancer:get_state(),
    ?assertMatch(empty, load_balancer:get()),
    load_balancer:check_health(),
    load_balancer:get_state(),
    ?assertMatch(empty, load_balancer:get()),
    load_balancer:stop().