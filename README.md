eredis + Poolboy
===================

=bai=

Status: Beta.

**eredis + Poolboy** provides connection pooling for
[eredis](//github.com/wooga/eredis) using
[Poolboy](//github.com/devinus/poolboy). It contains convenience functions for
executing Redis queries on a connection in a pool and lets you choose between two
methods for creating and managing connection pools:

1. Use it as a library that helps you supervise your own Redis connection pools.
2. Use it as an application that manages its own supervisor for connection pools.

I want to supervise my own connection pools
-------------------------------------------

Use `eredis_poolboy:child_spec/3` to get a child spec for a pool that you can use
in your own supervisor.

```Erlang
%% my own supervisor
init([]) ->
    PoolOptions  = [{size, 10}, {max_overflow, 20}],
    RedisOptions = [{host, "127.0.0.1"}, {port, 6379}, {database, 0},{password,"abc"}],
    ChildSpecs = [
        %% Redis pools
        eredis_poolboy:child_spec(pool1, PoolOptions, RedisOptions),
        %% other workers...
        {some_other_worker, {some_other_worker, start_link, []},
         permanent, 10, worker, [some_other_worker]}
    ],
    {ok, {{one_for_one, 10, 10}, ChildSpecs}}.
```

Let eredis + Poolboy supervise my pools
------------------------------------------

This approach requires you to start the application `eredis_poolboy`. Typically
this is done by adding `{applications, [eredis_poolboy]}` to your `.app.src`
file and then relying on your favourite release tool for the rest.

Pools can be added at run-time using `eredis_poolboy:add_pool/3`.

Pools can also be created at start-up by defining configuration parameters for
`eredis_poolboy`. The name of each configuration parameter is the pool name and
the value is a pair on the form `{PoolOptions, RedisOptions}`.

Example:

Start your Erlang node with `erl -config mypools.config` where `mypools.config`
is a file with the following contents:

```Erlang
{eredis_poolboy, [
    {pool1, {
      [{size, 10}, {max_overflow, 20}],
      [{host, "127.0.0.1"}, {port, 6379}, {database,0},{password,"abc"}]
    }}
]}
```

Using the connection pools
--------------------------

The most commonly used Redis functions are available with wrappers in
`eredis_poolboy`.

key-value set and get

```Erlang
1> eredis_pool:q(pool1, ["SET", "foo", "bar"]).
 {ok,<<"OK">>}
2> eredis_pool:q(pool1, ["GET", "foo"]).       
 {ok,<<"bar">>}
```

Redis Pipeline

```Erlang
3> Pipeline = [["SET", a, "1"],
      ["LPUSH", b, "3"],
      ["LPUSH", b, "2"]].
4> eredis_pool:qp(pool1, Pipeline).  
```

Use this as a dependency
------------------------

Using *erlang.mk*, put this in your `Makefile`:

```Erlang
DEPS = eredis_poolboy
dep_eredis_poolboy = git https://github.com/vzzy/eredis_poolboy 1.0.0
```

Using *rebar*, put this in your `rebar.config`:

```Erlang
{deps, [
    {eredis_poolboy, ".*", {git, "https://github.com/vzzy/eredis_poolboy",{tag, "1.0.0"}}}
]}.
```

License
-------

GNU Lesser General Public License (LGPL) version 3 or any later version.
Since the LGPL is a set of additional permissions on top of the GPL, both
license texts are included in the files [COPYING.LESSER](COPYING.LESSER) and
[COPYING](COPYING) respectively.
