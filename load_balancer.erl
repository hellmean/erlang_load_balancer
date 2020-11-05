-module(load_balancer).

-export([loop/3, register_provider/2, start/3, get/1, flip_active/2, get_message_router/3, health_checker/3]).
-import(provider, [start/2, find_flipped_health/3]).
-include("records.hrl").

loop(NodesCount, Nodes, NodesToVisit) ->
    receive
        {Client, Ref, register, Node=#provider{id=Id, is_active=IsActive, is_healthy=IsHealthy}} when NodesCount < 10 ->
            Client ! {self(), Ref, ok},
            if
                IsActive and IsHealthy ->
                    loop(NodesCount + 1, Nodes#{Id => Node}, [Node | NodesToVisit]);
                true ->
                    loop(NodesCount + 1, Nodes#{Id => Node}, NodesToVisit) 
            end;
        {Client, Ref, register, _} ->
            Client ! {self(), Ref, maximum_nodes_reached},
            loop(NodesCount, Nodes, NodesToVisit);
        {Client, Ref, get} when NodesToVisit =:= [] ->
            SuitableForVisit = [X || {_Key, X=#provider{is_active=IsActive, is_healthy=IsHealthy}} 
                <- maps:to_list(Nodes), IsActive and IsHealthy],
            case SuitableForVisit of 
                [Head | Tail] ->  
                    Head#provider.id ! {Client, Ref, get},
                    loop(NodesCount, Nodes, Tail);
                [] ->
                    Client ! {self(), Ref, no_healthy_active_nodes},
                    loop(NodesCount, Nodes, NodesToVisit)
            end;
        {Client, Ref, get} -> 
            [Head | Tail] = NodesToVisit,
            Head#provider.id ! {Client, Ref, get},
            loop(NodesCount, Nodes, Tail);
        {Client, Ref, get_nodes} ->
            Client ! {self(), Ref, Nodes},
            loop(NodesCount, Nodes, NodesToVisit);
        {Client, Ref, flip_active, Id} ->
            case maps:find(Id, Nodes) of
                {ok, X = #provider{is_active=IsActive}} ->
                    Client ! {self(), Ref, not IsActive},
                    loop(NodesCount, Nodes#{Id => X#provider{is_active=not IsActive}}, []);
                _ ->
                    Client ! {self(), Ref, not_found},
                    loop(NodesCount, Nodes, NodesToVisit)
            end
    end.

get_message_router(Server, Y, MsgMap) ->
    receive
        {Client, Ref, get} when Y > 0 ->
            Server ! {Client, Ref, get},
            get_message_router(Server, Y-1, MsgMap#{Ref => Client});
        {Client, Ref, get} when Y == 0 ->
            Client ! {self(), Ref, too_many_requests_in_progress},
            get_message_router(Server, Y, MsgMap);
        {_Provider, Ref, ok} ->
            #{Ref := Client} = MsgMap,
            Client ! {self(), Ref, ok},
            get_message_router(Server, Y+1, maps:remove(Ref, MsgMap))
    end.

register_provider(#load_balancer{load_balancer=Server}, Provider) ->
    Ref = make_ref(),
    Server ! {self(), Ref, register, Provider},
    receive
        {Server, Ref, ok} -> ok;
        {Server, Ref, maximum_nodes_reached} ->
            maximum_nodes_reached
    end.

get(#load_balancer{get_message_router=GetMessageRouter}) ->
    Ref = make_ref(),
    GetMessageRouter ! {self(), Ref, get},
    receive
        {GetMessageRouter, Ref, Result} -> Result
    end.

flip_active(#load_balancer{load_balancer=Server}, Id) ->
    Ref = make_ref(),
    Server ! {self(), Ref, flip_active, Id},
    receive
        {Server, Ref, Result} -> Result
    end.


%TODO: Not tested!
%TODO: not found cases, do we want to handle these?
health_checker(Server, CheckInterval, CheckTimeout) ->
    timer:sleep(CheckInterval),
    Ref = make_ref(),
    Server ! {self(), Ref, get_nodes},
    receive
        {Server, Ref, Nodes} ->
           Result = provider:find_flipped_health(Nodes, CheckTimeout, []),
           lists:map(fun(#provider{id=Id}) -> Server ! {self(), make_ref(), flip_active, Id} end, Result)
    end.

start(Y, HealthCheckInterval, HeathCheckTimeout) ->
    LoadBalancer = spawn(load_balancer, loop, [0, #{}, []]),
    GetMessageRouter = spawn(load_balancer, get_message_router, [LoadBalancer, Y, #{}]),
    _HeathChecker = spawn(load_balancer, health_checker, [LoadBalancer, HealthCheckInterval, HeathCheckTimeout]),
    #load_balancer{load_balancer=LoadBalancer, get_message_router=GetMessageRouter}.
