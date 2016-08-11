-module(eredis_poolboy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

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
        fun ({PoolName, {PoolArgs, MysqlArgs}}) ->
            eredis_poolboy:child_spec(PoolName, PoolArgs, MysqlArgs)
        end,
        Pools1
    ),
    {ok, { {one_for_one, 10, 10}, PoolSpec} }.

