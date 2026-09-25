-module(table_walk).
-export([start/0, keys/1, reader/1, writer/1]).

start() ->
    T = ets:new(book_table, [set, protected]),
    Values = [{N, lists:seq(1, N)} || N <- lists:seq(1, 5)],
    true = ets:insert(T, Values),
    T.

keys(T) ->
    true = ets:safe_fixtable(T, true),
    try
        Match = [{{'$1', '_'}, [], ['$1']}],
        lists:sort(chunks(ets:select(T, Match, 3), []))
    after ets:safe_fixtable(T, false)
    end.

chunks('$end_of_table', Acc) -> Acc;
chunks({Keys, Cont}, Acc) -> chunks(ets:select(Cont), Keys ++ Acc).

reader(T) -> other_process(fun() -> ets:lookup(T, 3) end).
writer(T) -> other_process(fun() -> ets:insert(T, {6, [6]}) end).

other_process(Fun) ->
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        Reply = try Fun() catch error:badarg -> {error, badarg} end,
        Parent ! {self(), Reply}
    end),
    receive
        {Pid, Reply} -> demonitor(Ref, [flush]), Reply;
        {'DOWN', Ref, process, Pid, Reason} -> error(Reason)
    after 1000 -> error(timeout)
    end.
