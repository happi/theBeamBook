#!/usr/bin/env escript
%%! +S 2 +JPcover true
-mode(compile).
main(_) ->
    Root = filename:dirname(filename:dirname(filename:dirname(
        filename:absname(escript:script_name())))),
    Load = fun(Path, Options) ->
        {ok, Module, Binary} = compile:file(filename:join(Root, Path), [binary|Options]),
        {module, Module} = code:load_binary(Module, Path, Binary),
        Module
    end,
    Load("code/io_chapter/pg_example/src/custom_io_server.erl", []),
    Load("code/io_chapter/pg_example/src/file_client.erl", []),
    {ok, Device} = file_client:open(),
    try
        ok = file_client:write(Device, "Hello, world!\n"),
        {ok, <<"Hello">>} = file_client:read(Device, 5),
        {ok, <<", world!\n">>} = file_client:read_line(Device),
        eof = file_client:read(Device, 1),
        {ok, <<"Hello, world!\n">>} = file_client:close(Device)
    after
        case is_process_alive(Device) of true -> file_client:close(Device); false -> ok end
    end,
    Load("code/io_chapter/socket_examples.erl", []),
    {ok, <<"pong">>} = socket_examples:tcp(inet),
    {ok, <<"pong">>} = socket_examples:tcp(socket),
    Parent = self(),
    Pid = spawn(fun() ->
        proc_lib:set_label({import, customers}),
        Parent ! {ready, self()},
        receive stop -> ok end
    end),
    Ref = monitor(process, Pid),
    try
        receive {ready, Pid} -> ok after 5000 -> error(label_timeout) end,
        {label, {import, customers}} = process_info(Pid, label),
        {import, customers} = proc_lib:get_label(Pid)
    after
        Pid ! stop,
        receive {'DOWN', Ref, process, Pid, normal} -> ok
        after 5000 -> error(worker_not_stopped) end
    end,
    jit = erlang:system_info(emu_flavor),
    line_counters = code:get_coverage_mode(),
    Load("code/beam_chapter/packet_demo/packet_demo.erl", [line_coverage]),
    Before = [{{encode,1},false},{{decode,1},false},{{retag,2},false}],
    Before = code:get_coverage(function, packet_demo),
    #{tag := 7, payload := <<10,20>>} = packet_demo:decode(<<7,10,20>>),
    [{{encode,1},false},{{decode,1},true},{{retag,2},false}] =
        code:get_coverage(function, packet_demo),
    [{6,0},{9,1},{12,0}] = code:get_coverage(line, packet_demo),
    ok = code:reset_coverage(packet_demo),
    Before = code:get_coverage(function, packet_demo),
    [{6,0},{9,0},{12,0}] = code:get_coverage(line, packet_demo),
    io:format("PASS: OTP ~s I/O cursor/EOF, both TCP backends, labels, native coverage and reset~n",
              [erlang:system_info(otp_release)]).
