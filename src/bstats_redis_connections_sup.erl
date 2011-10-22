-module(bstats_redis_connections_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_redis_connection/2]).

%% Supervisor callbacks
-export([init/1]).

%% =============================================================================
%% API functions
%% =============================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% =============================================================================
%% Supervisor callbacks
%% =============================================================================
-spec init([]) -> {ok, term()}.
init([]) ->
    RedisConnection = {eredis, {eredis, start_link, []},
                      transient, 5000, worker, [eredis]},

    {ok, {{simple_one_for_one, 5, 10}, [RedisConnection]}}.

start_redis_connection(IP, Port) ->
    supervisor:start_child(?MODULE, [IP, Port]).
