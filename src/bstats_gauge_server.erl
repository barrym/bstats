-module(bstats_gauge_server).

-behaviour(gen_server).

-include("bstats.hrl").

-export([
        start_link/1
    ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name = [], max = undefined, min = undefined, count = 0, sum = 0, enabled = true}).
-type state() :: #state{}.

-spec start_link(atom()) -> any().
start_link(Name) ->
    gen_server:start_link({local, ?GAUGE_SERVER_NAME(Name)}, ?MODULE, [Name], []).

-spec init([]) -> {ok, state()}.
init([Name]) ->
    bstats_redis:sadd("bstats:gauges", Name),
    timer:apply_interval(1000, gen_server, cast, [?GAUGE_SERVER_NAME(Name), write_to_redis]),
    {ok, #state{name = Name}}.

handle_info(_, State) ->
    {noreply, State}.

handle_call(get_info, _From, #state{name = Name, count = Count, sum = Sum, max = Max, min = Min, enabled = Enabled} = State) ->
    {reply, [{name, Name}, {count, Count}, {sum, Sum}, {max, Max}, {min, Min}, {enabled, Enabled}], State};

handle_call(disable_gauge, _From, #state{name = Name} = State) ->
    bstats_redis:srem("bstats:gauges", Name),
    {reply, ok, State#state{enabled = false}};

handle_call(enable_gauge, _From, #state{name = Name} = State) ->
    bstats_redis:sadd("bstats:gauges", Name),
    {reply, ok, State#state{enabled = true}};

handle_call(_Msg, _From, State) ->
    {reply, State, State}.

handle_cast({set_gauge, NewValue}, #state{count = Count, sum = Sum, max = Max, min = Min} = State) ->
    NewMax = case Max of
        undefined ->
            NewValue;
        _ ->
            case NewValue > Max of
                true ->
                    NewValue;
                false ->
                    Max
            end
    end,

    NewMin = case Min of
        undefined ->
            NewValue;
        _ ->
            case NewValue < Min of
                true ->
                    NewValue;
                false ->
                    Min
            end
    end,

    NewSum = Sum + NewValue,
    NewCount = Count + 1,
    {noreply, State#state{max = NewMax, min = NewMin, count = NewCount, sum = NewSum}};

handle_cast(write_to_redis, #state{name = Name, count = Count, sum = Sum, max = Max, min = Min, enabled = Enabled}) ->
    case Enabled of
        false ->
            ok;
        true ->
            Now = {Mega, Sec, _} = erlang:now(),
            {{_Year, _Month, _Day}, {_Hour, _Minute, Seconds}} = calendar:now_to_universal_time(Now),
            SecondTimestamp = (Mega * 1000000) + Sec,
            MinuteTimestamp = SecondTimestamp - Seconds,
            spawn(fun() ->
                        case Max of
                            undefined ->
                                do_nothing;
                            _ ->
                                % PER SECOND
                                SecondKey = "bstats:gauge:per_second:" ++ atom_to_list(Name),
                                write_redis_data(SecondKey, SecondTimestamp, 60 * ?PER_SECOND_LIMIT, Count, Sum, Max, Min),

                                % PER MINUTE
                                MinuteKey = "bstats:gauge:per_minute:" ++ atom_to_list(Name),
                                write_redis_data(MinuteKey, MinuteTimestamp, 3600 * ?PER_MINUTE_LIMIT, Count, Sum, Max, Min)

                                % PER HOUR
                                % HourKey = lists:flatten(io_lib:format("~w:~w~2.2.0w~2.2.0w~2.2.0w", [Name, Year, Month, Day, Hour])),
                                % write_redis_data(HourKey, 3600 * ?PER_HOUR_LIMIT, Count, Sum, Max, Min)
                        end
                end)
    end,
    {noreply, #state{name = Name, enabled = Enabled}};

handle_cast(del_gauge, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


write_redis_data(KeyFragment, Timestamp, Expire, Count, Sum, Max, Min) ->
    CountKey = lists:flatten(io_lib:format("~s:count:~p", [KeyFragment, Timestamp])),
    bstats_redis:incr_by(CountKey, Count),
    bstats_redis:expire(CountKey, Expire),

    SumKey = lists:flatten(io_lib:format("~s:sum:~p", [KeyFragment, Timestamp])),
    bstats_redis:incr_by(SumKey, round(Sum * 1000)),
    bstats_redis:expire(SumKey, Expire),

    MaxKey = lists:flatten(io_lib:format("~s:max:~p", [KeyFragment, Timestamp])),
    bstats_redis:make_transactioned_redis_request([
            ["ZADD", MaxKey, round(Max * 1000), round(Max * 1000)],
            ["ZREMRANGEBYRANK", MaxKey, 0, -2]
        ]),
    bstats_redis:expire(MaxKey, Expire),

    MinKey = lists:flatten(io_lib:format("~s:min:~p", [KeyFragment, Timestamp])),
    bstats_redis:make_transactioned_redis_request([
            ["ZADD", MinKey, round(Min * 1000), round(Min * 1000)],
            ["ZREMRANGEBYRANK", MinKey, 1, -1]
        ]),
    bstats_redis:expire(MinKey, Expire).

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{name = _Name}) ->
    % Not sure when on mutiple nodes
    % bstats_redis:del("bstats:counter" ++ atom_to_list(Name) ++ ":total"),
    % bstats_redis:srem("bstats:counters", Name),
    ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
