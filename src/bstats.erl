-module(bstats).

-include("bstats.hrl").

-export([
        inc_counter/1,
        inc_counter/2,
        del_counter/1,
        del_gauge/1,
        get_counter_info/1,
        get_gauge_info/1,
        set_gauge/2,
        register_all/0
    ]).


% lists:map(fun(_) -> spawn(fun() -> lists:foreach(fun(_) -> bstats:inc_counter(integer_to_list(crypto:rand_uniform(1,10))) end, lists:seq(1,10000)) end) end, lists:seq(1,100)).
% lists:map(fun(_) -> spawn(fun() -> lists:foreach(fun(_) -> bstats:inc_counter(foo) end, lists:seq(1,10000)) end) end, lists:seq(1,100)).

register_all() ->
    lists:foreach(fun(Counter) ->
                bstats_redis:sadd("bstats:counters", Counter)
        end, bstats_counter_sup:get_counters()),
    lists:foreach(fun(Gauge) ->
                bstats_redis:sadd("bstats:gauges", Gauge)
        end, bstats_gauge_sup:get_gauges()).

inc_counter(Name) ->
    inc_counter(Name, 1).

inc_counter(Name, By) when is_list(Name) ->
    inc_counter(list_to_atom(Name), By);
inc_counter(Name, By) ->
    cast_or_start(Name, counter, {inc_counter, By}).

set_gauge(Name, Value) when is_list(Name) ->
    set_gauge(list_to_atom(Name), Value);
set_gauge(Name, Value) ->
    cast_or_start(Name, gauge, {set_gauge, Value}).

del_counter(Name) ->
    gen_server:cast(?COUNTER_SERVER_NAME(Name), del_counter).

del_gauge(Name) ->
    gen_server:cast(?GAUGE_SERVER_NAME(Name), del_counter).

get_counter_info(Name) ->
    gen_server:call(?COUNTER_SERVER_NAME(Name), get_info).

get_gauge_info(Name) ->
    gen_server:call(?GAUGE_SERVER_NAME(Name), get_info).

% call_or_start(Name, Args) ->
%     try
%         gen_server:call(?COUNTER_SERVER_NAME(Name), Args)
%     catch exit:{noproc, {gen_server, call, _}} ->
%         bstats_sup:start_server(Name),
%         call_or_start(Name, Args)
%     end.

cast_or_start(Name, Type, Args) ->
    case Type of
        counter ->
            case whereis(?COUNTER_SERVER_NAME(Name)) of
                undefined ->
                    bstats_counter_sup:start_server(Name),
                    cast_or_start(Name, Type, Args);
                _ ->
                    gen_server:cast(?COUNTER_SERVER_NAME(Name), Args)
            end;
        gauge ->
            case whereis(?GAUGE_SERVER_NAME(Name)) of
                undefined ->
                    bstats_gauge_sup:start_server(Name),
                    cast_or_start(Name, Type, Args);
                _ ->
                    gen_server:cast(?GAUGE_SERVER_NAME(Name), Args)
            end
    end.
