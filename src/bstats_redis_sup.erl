-module(bstats_redis_sup).

-behaviour(supervisor).

-include("bstats.hrl").

%% API
-export([
        start_link/0
    ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% =============================================================================
%% API functions
%% =============================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% =============================================================================
%% Supervisor callbacks
%% =============================================================================
-spec init(list()) -> {ok, term()}.
init([]) ->
    BstatsRedis = {bstats_redis, {bstats_redis, start_link, []},
                          permanent, 30000, worker, [bstats_redis]},

    {ok, {{one_for_one, 5, 10}, [BstatsRedis]}}.
