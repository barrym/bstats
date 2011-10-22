-module(bstats_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% =============================================================================
%% Application callbacks
%% =============================================================================
-spec start(normal | {takeover, node()} | {failover, node()}, terms) ->
    {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
    bstats_counter_sup:start_link(),
    bstats_gauge_sup:start_link(),
    bstats_redis_connections_sup:start_link(),
    bstats_redis_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
