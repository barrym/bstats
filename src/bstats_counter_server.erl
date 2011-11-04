-module(bstats_counter_server).

-behaviour(gen_server).

-include("bstats.hrl").

-export([
        start_link/2
    ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {namespace = [], counter = [], counter_value = 0, previous_counter_value = 0, enabled = true}).
-type state() :: #state{}.

-spec start_link(atom(), atom()) -> any().
start_link(Namespace, Counter) ->
    gen_server:start_link({local, ?COUNTER_SERVER_NAME(Namespace, Counter)}, ?MODULE, [Namespace, Counter], []).

-spec init([]) -> {ok, state()}.
init([Namespace, Counter]) ->
    bstats_redis:sadd("bstats:namespaces", Namespace),
    bstats_redis:sadd("bstats:" ++ atom_to_list(Namespace) ++ ":counters", Counter),
    timer:apply_interval(1000, gen_server, cast, [?COUNTER_SERVER_NAME(Namespace, Counter), write_to_redis]),
    {ok, #state{namespace = Namespace, counter = Counter}}.

handle_info(_, State) ->
    {noreply, State}.

handle_call(get_info, _From, #state{namespace = Namespace, counter = Counter, counter_value = CounterValue, enabled = Enabled} = State) ->
    {reply, [{namespace, Namespace}, {counter, Counter}, {counter_value, CounterValue}, {enabled, Enabled}], State};

handle_call(disable_counter, _From, #state{namespace = Namespace, counter = Counter} = State) ->
    bstats_redis:srem("bstats:" ++ atom_to_list(Namespace) ++ ":counters", Counter),
    {reply, ok, State#state{enabled = false}};

handle_call(enable_counter, _From, #state{namespace = Namespace, counter = Counter} = State) ->
    bstats_redis:sadd("bstats:" ++ atom_to_list(Namespace) ++ ":counters", Counter),
    {reply, ok, State#state{enabled = true}};

handle_call(_Msg, _From, State) ->
    {reply, State, State}.

handle_cast({inc_counter, By}, #state{counter_value = CounterValue} = State) ->
    {noreply, State#state{counter_value = CounterValue + By}};

handle_cast(write_to_redis, #state{namespace = Namespace, counter = Counter, counter_value = CounterValue, previous_counter_value = PreviousValue, enabled = Enabled} = State) ->
    case Enabled of
        false ->
            ok;
        true ->
            Now = {Mega, Sec, _} = erlang:now(),
            {{_Year, _Month, _Day}, {_Hour, Minutes, Seconds}} = calendar:now_to_universal_time(Now),
            SecondTimestamp = (Mega * 1000000) + Sec,
            MinuteTimestamp = SecondTimestamp - Seconds,
            HourTimestamp   = MinuteTimestamp - 60 * Minutes,
            spawn(fun() ->
                        Delta = CounterValue - PreviousValue,

                        SecondKey = "bstats:" ++ atom_to_list(Namespace) ++ ":counter:per_second:" ++ atom_to_list(Counter) ++ ":" ++ integer_to_list(SecondTimestamp),
                        bstats_redis:incr_by(SecondKey, Delta),
                        bstats_redis:expire(SecondKey, 60 * ?PER_SECOND_LIMIT),

                        MinuteKey = "bstats:" ++ atom_to_list(Namespace) ++ ":counter:per_minute:" ++ atom_to_list(Counter) ++ ":" ++ integer_to_list(MinuteTimestamp),
                        bstats_redis:incr_by(MinuteKey, Delta),
                        bstats_redis:expire(MinuteKey, 3600 * ?PER_MINUTE_LIMIT),

                        HourKey = "bstats:" ++ atom_to_list(Namespace) ++ ":counter:per_hour:" ++ atom_to_list(Counter) ++ ":" ++ integer_to_list(HourTimestamp),
                        bstats_redis:incr_by(HourKey, Delta),
                        bstats_redis:expire(HourKey, 3600 * ?PER_HOUR_LIMIT)
                end)
    end,
    {noreply, State#state{previous_counter_value = CounterValue}};

handle_cast(del_counter, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{namespace = _Namespace, counter = _Counter}) ->
    % Not sure when on mutiple nodes
    % bstats_redis:del("bstats:counter" ++ atom_to_list(Name) ++ ":total"),
    % bstats_redis:srem("bstats:counters", Name),
    ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
