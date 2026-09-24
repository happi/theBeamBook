#!/usr/bin/env escript
%%! +S 2
-mode(compile).

main([]) ->
    "29" = erlang:system_info(otp_release),
    lists:foreach(fun({Name, Test}) -> Test(), io:format("PASS ~s~n", [Name]) end,
                  [{iterator, fun iterator/0},
                   {numeric_comparison, fun numeric_comparison/0},
                   {ets_lifetime, fun ets_lifetime/0},
                   {priority_monitor, fun priority_monitor/0},
                   {priority_link_trapping, fun priority_link_trapping/0},
                   {priority_link_nontrapping, fun priority_link_nontrapping/0},
                   {local_pid_roundtrip, fun local_pid_roundtrip/0}]).

iterator() ->
    Pid = spawn(fun() -> receive stop -> ok end end),
    try
        Pids = collect(erlang:processes_iterator(), []),
        true = lists:member(self(), Pids),
        true = lists:member(Pid, Pids),
        true = lists:all(fun is_pid/1, Pids)
    after Pid ! stop end.

collect(Iter, Acc) ->
    case erlang:processes_next(Iter) of
        {Pid, Next} -> collect(Next, [Pid | Acc]);
        none -> Acc
    end.

numeric_comparison() ->
    I = list_to_integer("9999999999999999"),
    F = list_to_float("1.0e16"),
    false = (I == F), true = (I < F),
    true = (1 == 1.0), false = (1 =:= 1.0).

ets_lifetime() ->
    Parent = self(),
    {Owner, Ref} = spawn_monitor(fun() ->
        Plain = ets:new(plain, [set, public]),
        Inherited = ets:new(inherited, [set, {heir, Parent, inherited}]),
        true = ets:insert(Plain, {key, "a string"}),
        Parent ! {tables, Plain, Inherited},
        receive stop -> ok end
    end),
    receive {tables, Plain, Inherited} ->
        Owner = ets:info(Plain, owner),
        false = ets:info(Plain, compressed),
        [{key, "a string"}] = ets:lookup(Plain, key),
        Owner ! stop,
        receive {'DOWN', Ref, process, Owner, normal} -> ok after 5000 -> error(owner_timeout) end,
        undefined = ets:info(Plain),
        receive {'ETS-TRANSFER', Inherited, Owner, inherited} -> ok after 5000 -> error(heir_timeout) end,
        Parent = ets:info(Inherited, owner),
        true = ets:delete(Inherited)
    after 5000 -> error(table_timeout) end.

priority_monitor() ->
    Pid = spawn(fun() -> receive stop -> ok end end),
    Ref = erlang:monitor(process, Pid, [priority]),
    self() ! ordinary,
    Pid ! stop,
    Msg = {'DOWN', Ref, process, Pid, normal},
    await_queued(Msg, 500),
    Msg = next(),
    ordinary = next().

priority_link_trapping() ->
    Previous = process_flag(trap_exit, true),
    try
        Pid = spawn(fun() -> receive stop -> exit(test_exit) end end),
        true = erlang:link(Pid, [priority]),
        self() ! ordinary,
        Pid ! stop,
        Msg = {'EXIT', Pid, test_exit},
        await_queued(Msg, 500),
        Msg = next(),
        ordinary = next()
    after process_flag(trap_exit, Previous) end.

priority_link_nontrapping() ->
    {Pid, Ref} = spawn_monitor(fun() ->
        Target = spawn(fun() -> receive stop -> exit(test_exit) end end),
        true = erlang:link(Target, [priority]),
        Target ! stop,
        receive {'EXIT', _, _} -> exit(unexpected_exit_message) end
    end),
    receive {'DOWN', Ref, process, Pid, test_exit} -> ok
    after 5000 -> error(priority_exit_timeout) end.

%% Wait for signal conversion before testing an unselective receive. No alias
%% is created anywhere in these priority tests.
await_queued(Msg, N) when N > 0 ->
    {messages, Messages} = process_info(self(), messages),
    case lists:member(Msg, Messages) of
        true -> ok;
        false -> timer:sleep(10), await_queued(Msg, N - 1)
    end;
await_queued(Msg, 0) -> error({queue_timeout, Msg}).

next() -> receive Msg -> Msg after 5000 -> error(receive_timeout) end.

local_pid_roundtrip() ->
    Pid = self(),
    Pid = binary_to_term(term_to_binary(Pid)),
    true = is_pid(Pid),
    true = (node(Pid) =:= node()).
