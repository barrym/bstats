-module(bstats_redis).

-behaviour(gen_server).

-include("bstats.hrl").

-export([
        start_link/0,
        sadd/2,
        srem/2,
        incr_by/2,
        expire/2,
        make_transactioned_redis_request/1
    ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {connection, redis_up = false, host = undefined, port = undefined, reconnecting = false}).
-type state() :: #state{}.

%% API
sadd(Key, Member) ->
    gen_server:call(?MODULE, {sadd, Key, Member}).

srem(Key, Member) ->
    gen_server:call(?MODULE, {srem, Key, Member}).

incr_by(Key, Value) ->
    gen_server:call(?MODULE, {incr_by, Key, Value}).

expire(Key, Seconds) ->
    gen_server:call(?MODULE, {expire, Key, Seconds}).

make_transactioned_redis_request(Commands) ->
    gen_server:call(?MODULE, {make_transactioned_redis_request, Commands}).

% GEN SERVER
-spec start_link() -> any().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, Host} = application:get_env(bstats, host),
    {ok, Port} = application:get_env(bstats, port),
    case bstats_redis_connections_sup:start_redis_connection(Host, Port) of
        {ok, Connection} ->
            {ok, #state{connection = Connection, host = Host, port = Port, redis_up = true}};
        {error, _Reason} ->
            timer:apply_after(1000, gen_server, cast, [?MODULE, reconnect]),
            {ok, #state{host = Host, port = Port, reconnecting = true}}
    end.

handle_info(_, State) ->
    {noreply, State}.

handle_call({sadd, Key, Member}, _From, State) ->
    {Result, NewState} = make_redis_request(["SADD", Key, Member], State),
    {reply, Result, NewState};

handle_call({srem, Key, Member}, _From, State) ->
    {Result, NewState} = make_redis_request(["SREM", Key, Member], State),
    {reply, Result, NewState};

handle_call({incr_by, Key, Value}, _From, State) ->
    {Result, NewState} = make_redis_request(["INCRBY", Key, Value], State),
    {reply, Result, NewState};

handle_call({expire, Key, Seconds}, _From, State) ->
    {Result, NewState} = make_redis_request(["EXPIRE", Key, Seconds], State),
    {reply, Result, NewState};

handle_call({make_transactioned_redis_request, Commands}, _From, State) ->
    {Result, NewState} = make_transactioned_redis_request(Commands, State),
    {reply, Result, NewState};

handle_call(_Msg, _From, State) ->
    {reply, State, State}.

handle_cast(reconnect, #state{host = Host, port = Port} = State) ->
    case bstats_redis_connections_sup:start_redis_connection(Host, Port) of
        {ok, Connection} ->
            {noreply, State#state{connection = Connection, redis_up = true, reconnecting = false}};
        {error, _Reason} ->
            timer:apply_after(1000, gen_server, cast, [?MODULE, reconnect]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{}) ->
    ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

make_redis_request(Command, #state{redis_up = RedisUp, reconnecting = Reconnecting} = State) ->
    case RedisUp of
        true ->
            make_redis_request_internal(Command, State);
        false ->
            case Reconnecting of
                true ->
                    ok;
                false ->
                    timer:apply_after(1000, gen_server, cast, [?MODULE, reconnect])
            end,
            {{error, no_connection}, State#state{redis_up = false, connection = undefined, reconnecting = true}}
    end.

make_transactioned_redis_request(Commands, #state{redis_up = RedisUp, reconnecting = Reconnecting} = State) ->
    case RedisUp of
        true ->
            make_redis_request_internal(["MULTI"], State),
            lists:foreach(fun(Command) ->
                        make_redis_request_internal(Command, State)
                end, Commands),
            make_redis_request_internal(["EXEC"], State);
        false ->
            case Reconnecting of
                true ->
                    ok;
                false ->
                    timer:apply_after(1000, gen_server, cast, [?MODULE, reconnect])
            end,
            {{error, no_connection}, State#state{redis_up = false, connection = undefined, reconnecting = true}}
    end.

make_redis_request_internal(Command, #state{connection = Connection} = State) ->
    case eredis:q(Connection, Command) of
        {error, no_connection} ->
            {{error, no_connection}, State#state{redis_up = false, connection = undefined}};
        EredisResult ->
            {EredisResult, State}
    end.
