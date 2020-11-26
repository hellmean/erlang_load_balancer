-module(health_checker).
-export([loop/1, start_link/1]).

loop(WaitInterval) ->
    timer:sleep(WaitInterval),
    load_balancer:check_health(),
    loop(WaitInterval).

start_link(WaitInterval) ->
    Pid = spawn_link(?MODULE, loop, [WaitInterval]),
    {ok, Pid}.
