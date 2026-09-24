-module(book_diagnostics).
-export([large_heap/0, session_heap/0, schedulers/0, echo/1, trace_echo/0,
         retained_slice/0]).

%% Each example owns its worker and restores the diagnostic state it changes.
% tag::monitor[]
large_heap() ->
    Previous = erlang:system_monitor(self(), [{large_heap, 10000}]),
    try
        Worker = spawn(fun heap_worker/0),
        try
            receive
                {monitor, Worker, large_heap, Info} -> Info
            after 5000 -> error(no_heap_event)
            end
        after
            exit(Worker, kill)
        end
    after
        erlang:system_monitor(Previous)
    end.
% end::monitor[]

% tag::session[]
session_heap() ->
    Session = trace:session_create(book_heap, self(), []),
    try
        trace:system(Session, large_heap, 10000),
        Worker = spawn(fun heap_worker/0),
        try
            receive
                {monitor, Worker, large_heap, Info} -> Info
            after 5000 -> error(no_session_event)
            end
        after
            exit(Worker, kill)
        end
    after
        trace:session_destroy(Session)
    end.
% end::session[]

% tag::heap-worker[]
heap_worker() ->
    Data = lists:seq(1, 100000),
    erlang:garbage_collect(),
    receive {fetch, From} -> From ! Data end.
% end::heap-worker[]

% tag::schedulers[]
schedulers() ->
    erlang:system_flag(scheduler_wall_time, true),
    try
        First = scheduler:sample_all(),
        timer:sleep(1000),
        scheduler:utilization(First, scheduler:sample_all())
    after
        erlang:system_flag(scheduler_wall_time, false)
    end.
% end::schedulers[]

echo(Value) -> Value.

%% Run on a disposable node: dbg uses its own server and trace settings.
% tag::trace[]
trace_echo() ->
    {ok, _} = dbg:tracer(),
    try
        {ok, _} = dbg:p(self(), [call]),
        {ok, _} = dbg:tpl(?MODULE, echo, 1,
                          [{[42], [], [{return_trace}]}]),
        41 = ?MODULE:echo(41),
        42 = ?MODULE:echo(42),
        ok
    after
        dbg:stop()
    end.
% end::trace[]

retained_slice() ->
    Big = binary:copy(<<0>>, 1000000),
    Part = binary:part(Big, 0, 100),
    Copy = binary:copy(Part),
    {byte_size(Part), binary:referenced_byte_size(Part),
     byte_size(Copy), binary:referenced_byte_size(Copy)}.
