-module(process_walk).
-export([start/0, drain/1, labels/0, stop/1]).

start() ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        proc_lib:set_label({book_worker, 1}),
        Alias = alias([priority]),
        Parent ! {Ref, self(), Alias},
        loop()
    end),
    receive {Ref, Pid, Alias} -> {Pid, Alias} end.

loop() ->
    receive
        {drain, From, Ref} ->
            From ! {Ref, pending([])},
            loop();
        stop -> ok
    end.

pending(Acc) ->
    receive
        {work, _} = Message -> pending([Message | Acc])
    after 0 -> lists:reverse(Acc)
    end.

drain(Pid) ->
    Ref = make_ref(),
    Pid ! {drain, self(), Ref},
    receive {Ref, Messages} -> Messages after 1000 -> error(timeout) end.

labels() -> labels(erlang:processes_iterator(), []).
labels(Iter, Acc) ->
    case erlang:processes_next(Iter) of
        {Pid, Next} ->
            case process_info(Pid, label) of
                {label, {book_worker, _} = Label} ->
                    labels(Next, [{Pid, Label} | Acc]);
                _ -> labels(Next, Acc)
            end;
        none -> Acc
    end.

stop(Pid) ->
    Ref = monitor(process, Pid),
    Pid ! stop,
    receive {'DOWN', Ref, process, Pid, Reason} -> Reason
    after 1000 -> error(timeout)
    end.
