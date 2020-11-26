# erlang_load_balancer

Hobby project to learn Erlang and OTP. A round robin load balancer.

In order to use it you need to install rebar3 (https://github.com/erlang/rebar3).

Run `# rebar3 compile` to compile.

Run `# rebar3 eunit` to run the tests.

Now you can play with it using `# rebar3 shell`. This will start both the load balancer and the health checker, that will check the health of all registered nodes every 5 seconds.

If you want to explore how the load balancer works it is best to terminate the health_checker first:
```
1> supervisor:terminate_child(erlang_load_balancer_sup, health_checker).
ok
```

Now you can try a get request on the load_balancer:

```
2> load_balancer:get().
empty
```

The the full list of possible commands under the API section in `src/load_balancer.erl`. And check the `test/load_balancer_test.erl` to see how they can be used. 

Two test provider nodes are implemented: `my_debug_provider` (its heath alternates between `true` and `false` upon each health check), and `my_debug_provider_with_stack` (accepts a stack of heath values and emits them one by one on health check request, onces the stack is empty, only `true` values are emitted). Other providers can implemented using the `provider` behaviour, including those that will actually do some TCP/IP requests. 

You can create an instance of a node and register it to a provider like this:

```
3> P1 = my_debug_provider_with_stack:start(777, [true,false]).
<0.159.0>
4> load_balancer:register(P1).
ok
```

Now you can try issuing `load_balancer:health_check()` and `load_balancer:get()` multiple times to see what happens.

Finally, you can restart the rebar3 shell to bring back the health checker (or manually start it) and register some providers again do `load_balancer:get()` every couple of seconds to see how it works with the health checker running.