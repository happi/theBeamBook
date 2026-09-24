#!/usr/bin/env escript
%%! +S 2
-mode(compile).
main(_) ->
    Source = filename:join(filename:dirname(escript:script_name()),
                           "book_diagnostics.erl"),
    {ok, book_diagnostics, Bin} = compile:file(Source, [binary, debug_info]),
    {module, book_diagnostics} = code:load_binary(book_diagnostics, Source, Bin),
    Previous = erlang:system_monitor(),
    Info = book_diagnostics:large_heap(),
    true = proplists:get_value(heap_block_size, Info) +
           proplists:get_value(old_heap_block_size, Info) >= 10000,
    Previous = erlang:system_monitor(),
    Before = trace:session_info(all),
    SessionInfo = book_diagnostics:session_heap(),
    true = proplists:is_defined(heap_size, SessionInfo),
    Before = trace:session_info(all),
    Previous = erlang:system_monitor(),
    Util = book_diagnostics:schedulers(),
    true = lists:any(fun({io, _, _, _}) -> true; (_) -> false end, Util),
    true = lists:any(fun({normal, _, _, _}) -> true; (_) -> false end, Util),
    true = length(erlang:statistics(run_queue_lengths)) =:=
           erlang:system_info(schedulers) + 1,
    {100, 1000000, 100, 100} = book_diagnostics:retained_slice(),
    true = is_integer(erlang:memory(ets)),
    ok = book_diagnostics:trace_echo(),
    io:format("PASS: monitor threshold/restoration, session event/cleanup, "
              "scheduler types, queue count, binary retention, ETS metric, "
              "conditional call/return trace~n").
