-module(load_balancer).

-behaviour(gen_server).

-export([check_health/0, exclude/1, get/0, get_state/0, handle_call/3, handle_cast/2,
         include/1, init/1, register/1, report_back_health/2, start_link/1, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% misc helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_provider_from_list(Target, Providers) ->
    F = fun(X) -> X =:= Target end,
    {Found, Rest} = lists:partition(F, Providers),
    {Found, Rest}.

exclude_provider_in_state(Provider, {Count, Seen, NotSeen, Excluded}) ->
    {FoundInSeen, FilteredSeen} = extract_provider_from_list(Provider, Seen),
    {FoundInNotSeen, FilteredNotSeen} = extract_provider_from_list(Provider, NotSeen),
    NewExcluded =
        lists:map(fun ({_Health, X}) when X =:= Provider ->
                          {0, Provider};
                      (Other) ->
                          Other
                  end,
                  Excluded),
    Found = FoundInNotSeen ++ FoundInSeen,
    ToExclude = [{0, X} || X <- Found],
    {Found, {Count, FilteredSeen, FilteredNotSeen, ToExclude ++ NewExcluded}}.

include_provider_in_state(Provider, {Count, Seen, NotSeen, Excluded}, HealthCutOff) ->
    NewExcluded =
        lists:map(fun ({Health, X}) when X =:= Provider, Health >= HealthCutOff ->
                          {HealthCutOff, Provider};
                      ({Health, X}) when X =:= Provider ->
                          {Health + 1, Provider};
                      (Other) ->
                          Other
                  end,
                  Excluded),
    {Found, RestExcluded} = extract_provider_from_list({HealthCutOff, Provider}, NewExcluded),
    ToInclude = [X || {_Health, X} <- Found],
    {ToInclude, {Count, Seen, ToInclude ++ NotSeen, RestExcluded}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(NodeLimit) ->
    {ok, {NodeLimit, [], [], []}}.

handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};
handle_call({register, _}, _From, State = {0, _, _, _}) ->
    {reply, limit_exceeded, State};
handle_call({register, Provider}, _From, {Count, Seen, NotSeen, Excluded}) ->
    {reply, ok, {Count - 1, Seen, [Provider | NotSeen], Excluded}};
handle_call(get, _From, State = {_, [], [], _}) ->
    {reply, empty, State};
handle_call(get, _From, {Count, Seen, [Head | NotSeenTail], Excluded}) ->
    {reply, {ok, Head}, {Count, [Head | Seen], NotSeenTail, Excluded}};
handle_call(get, _From, {Count, Seen, [], Excluded}) ->
    [Head | Tail] = lists:reverse(Seen),
    {reply, {ok, Head}, {Count, [Head], Tail, Excluded}};
handle_call({exclude, Provider}, _From, State) ->
    {Excluded, NewState} = exclude_provider_in_state(Provider, State),
    {reply, {excluded, Excluded}, NewState};
handle_call({include, Provider}, _From, State) ->
    {Included, NewState} = include_provider_in_state(Provider, State, 0),
    {reply, {included, Included}, NewState}.

handle_cast(check_health, State = {_, Seen, NotSeen, Excluded}) ->
    F = fun(Provider) -> provider:check_cast(Provider) end,
    lists:foreach(F, Seen),
    lists:foreach(F, NotSeen),
    lists:foreach(fun({_Health, Provider}) -> provider:check_cast(Provider) end, Excluded),
    {noreply, State};
handle_cast({health_report, Provider, false}, State) ->
    {_, NewState} = exclude_provider_in_state(Provider, State),
    {noreply, NewState};
handle_cast({health_report, Provider, true}, State) ->
    {_, NewState} = include_provider_in_state(Provider, State, 2),
    {noreply, NewState};
handle_cast(_, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(NodeLimit) ->
    gen_server:start_link({local, load_balancer}, load_balancer, NodeLimit, []).

report_back_health(Provider, Report) ->
    gen_server:cast(load_balancer, {health_report, Provider, Report}).

register(Provider) ->
    gen_server:call(load_balancer, {register, Provider}).

get() ->
    case gen_server:call(load_balancer, get) of
        {ok, Provider} ->
            provider:get(Provider);
        empty ->
            empty
    end.

exclude(Provider) ->
    gen_server:call(load_balancer, {exclude, Provider}).

include(Provider) ->
    gen_server:call(load_balancer, {include, Provider}).

check_health() ->
    gen_server:cast(load_balancer, check_health).

get_state() ->
    gen_server:call(load_balancer, state).

stop() ->
    gen_server:call(load_balancer, stop).
