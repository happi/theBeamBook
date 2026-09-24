%% A server that keeps its own memory in check: it collects garbage
%% after a fixed number of requests and hibernates when idle, so that
%% the garbage of a burst does not sit on the heap until the next one.
-module(tweak_hoard).
-export([start/0, loop/2]).

-define(GC_EVERY, 1000).
-define(IDLE_MS, 5000).

start() -> spawn(?MODULE, loop, [#{}, 0]).

loop(State, N) when N >= ?GC_EVERY ->
    garbage_collect(),
    loop(State, 0);
loop(State, N) ->
    receive
        {put, Key, Value} ->
            loop(State#{Key => Value}, N + 1);
        {get, From, Key} ->
            From ! {ok, maps:get(Key, State, undefined)},
            loop(State, N + 1);
        stop ->
            ok
    after ?IDLE_MS ->
        erlang:hibernate(?MODULE, loop, [State, 0])
    end.
