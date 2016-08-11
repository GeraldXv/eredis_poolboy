%% @author bai
%% @doc @todo Add description to eredis_poolboy.


-module(eredis_poolboy).
-define(TIMEOUT, 10*1000).
%% ====================================================================
%% API functions
%% ====================================================================
-export([
	add_pool/3,
	checkin/2,
	checkout/1,
	child_spec/3,
	
	q/2, 
	q/3, 
	qp/2, 
	qp/3, 
	q_noreply/2		 
]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Adds a pool to the started eredis_poolboy application.
add_pool(PoolName, PoolArgs, RedisArgs) ->
    %% We want strategy fifo as default instead of lifo.
    PoolSpec = child_spec(PoolName, PoolArgs, RedisArgs),
    supervisor:start_child(eredis_poolboy_sup, PoolSpec).

%% @doc Returns a redis connection to the given pool.
checkin(PoolName, Connection) ->
    poolboy:checkin(PoolName, Connection).

%% @doc Checks out a redis connection from a given pool.
checkout(PoolName) ->
    poolboy:checkout(PoolName).

%% @doc Creates a supvervisor:child_spec. When the need to
%% supervise the pools in another way.
child_spec(PoolName, PoolArgs, RedisArgs) ->
    PoolArgs1 = case proplists:is_defined(strategy, PoolArgs) of
        true  ->
            [{name, {local, PoolName}}, {worker_module, eredis} | PoolArgs];
        false ->
            %% Use fifo by default. Redis closes unused connections after a certain time.
            %% Fifo causes all connections to be regularily used which prevents them from
            %% being closed.,
            [{strategy, fifo}, {name, {local, PoolName}}, {worker_module, eredis} | PoolArgs]
    end,
    poolboy:child_spec(PoolName, PoolArgs1, RedisArgs).

-spec q(PoolName::atom(), Command::iolist()) -> {ok, binary() | [binary()]} | {error, Reason::binary()}.
q(PoolName, Command) ->
    q(PoolName, Command, ?TIMEOUT).

-spec q(PoolName::atom(), Command::iolist(), Timeout::integer()) -> {ok, binary() | [binary()]} | {error, Reason::binary()}.
q(PoolName, Command, Timeout) ->
    poolboy:transaction(PoolName, fun(Worker) ->
    	eredis:q(Worker, Command, Timeout)
    end).

-spec qp(PoolName::atom(), Command::iolist(), Timeout::integer()) -> {ok, binary() | [binary()]} | {error, Reason::binary()}.
qp(PoolName, Pipeline) ->
    qp(PoolName, Pipeline, ?TIMEOUT).

qp(PoolName, Pipeline, Timeout) ->
    poolboy:transaction(PoolName, fun(Worker) ->
   		eredis:qp(Worker, Pipeline, Timeout)
    end).

-spec q_noreply(PoolName::atom(), Command::iolist()) -> ok.
q_noreply(PoolName, Command) ->
    poolboy:transaction(PoolName, fun(Worker) ->
    	eredis:q_noreply(Worker, Command)
    end).

