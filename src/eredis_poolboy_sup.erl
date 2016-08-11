-module(eredis_poolboy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Pools = application:get_all_env(eredis_poolboy),
    Pools1 = proplists:delete(included_applications, Pools),
    PoolSpec = lists:map(
        fun ({PoolName, {PoolArgs, RedisArgs}}) ->
            eredis_poolboy:child_spec(PoolName, PoolArgs, RedisArgs)
        end,
        Pools1
    ),
    {ok, { {one_for_one, 10, 10}, PoolSpec} }.

