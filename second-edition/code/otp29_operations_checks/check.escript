#!/usr/bin/env escript
%%! +S 2 -sname bb_operations_check
-mode(compile).
main([Tmp]) ->
    "29" = erlang:system_info(otp_release),
    hot_funs(),
    distribution(),
    ssh_shell(Tmp),
    Log = filename:join(Tmp, "ct_logs"),
    ok = file:make_dir(Log),
    {1, 0, {0, 0}} = ct:run_test([{dir, filename:absname("code/otp29_operations_checks")},
                                 {suite, operations_SUITE}, {verbosity, 100}, {logdir, Log}]),
    io:format("PASS Common Test headers and verbosity 100~n"),
    {ok, Chapter} = file:read_file("chapters/21_testing.asciidoc"),
    [_, SampleTail] = binary:split(Chapter, <<"-module(sample_SUITE).">>),
    [SampleBody, _] = binary:split(SampleTail, <<"```">>),
    ok = file:write_file(filename:join(Tmp, "sample_SUITE.erl"),
                         <<"-module(sample_SUITE).", SampleBody/binary>>),
    SampleLog = filename:join(Tmp, "sample_logs"),
    ok = file:make_dir(SampleLog),
    {1, 0, {0, 0}} = ct:run_test([{dir, Tmp}, {suite, sample_SUITE},
                                 {verbosity, 100}, {logdir, SampleLog}]),
    io:format("PASS exact Common Test suite extracted from manuscript~n").
hot_funs() ->
    Compile = fun(N) ->
        {ok, hot_fun_demo, B} = compile:file("code/ops_chapter/src/hot_fun_demo.erl",
                                            [binary, {d, 'INCREMENT', N}]), B
    end,
    V1 = Compile(1), V2 = Compile(2), V3 = Compile(3),
    {module, hot_fun_demo} = code:load_binary(hot_fun_demo, "v1", V1),
    {F, L} = hot_fun_demo:funs(),
    {11, 11} = {F(10), L(10)},
    {module, hot_fun_demo} = code:load_binary(hot_fun_demo, "v2", V2),
    {12, 11} = {F(10), L(10)},
    false = code:purge(hot_fun_demo),
    old_fun_purged = try L(10) catch error:{badfun, _} -> old_fun_purged end,
    12 = F(10),
    {module, hot_fun_demo} = code:load_binary(hot_fun_demo, "v3", V3),
    13 = F(10),
    {_, L3} = hot_fun_demo:funs(),
    {module, hot_fun_demo} = code:load_binary(hot_fun_demo, "v1", V1),
    13 = L3(10),
    {module, hot_fun_demo} = code:load_binary(hot_fun_demo, "v2", V2),
    old_fun_purged = try L3(10) catch error:{badfun, _} -> old_fun_purged end,
    12 = F(10),
    io:format("PASS external/local funs across three versions, purge and badfun~n").
distribution() ->
    {ok, Peer, Node} = peer:start_link(#{name => bb_operations_peer, args => ["+S", "2"]}),
    try
        true = lists:member($@, atom_to_list(Node)),
        Remote = rpc:call(Node, erlang, whereis, [init]),
        Node = node(Remote),
        Me = self(),
        {Me, Remote} = rpc:call(Node, erlang, binary_to_term, [term_to_binary({Me, Remote})]),
        Ref = make_ref(),
        true = rpc:cast(Node, erlang, send, [Me, {cast_ran, Ref}]),
        receive {cast_ran, Ref} -> ok after 5000 -> error(cast_timeout) end,
        %% rex suspension distinguishes erpc-backed ordinary calls from block_call.
        Rex = rpc:call(Node, erlang, whereis, [rex]),
        ok = sys:suspend(Rex, 5000),
        try Node = rpc:call(Node, erlang, node, [], 5000)
        after ok = sys:resume(Rex, 5000) end,
        io:format("PASS short node host, local/remote PID roundtrip, RPC without rex, cast true and delivery~n")
    after peer:stop(Peer) end.
ssh_shell(Tmp) ->
    ok = ssh:start(),
    Password = binary_to_list(base64:encode(crypto:strong_rand_bytes(24))),
    {ok, D} = ssh:daemon({127,0,0,1}, 0,
                        [{system_dir, Tmp}, {auth_methods, "password"},
                         {user_passwords, [{"beam_test", Password}]},
                         {shell, fun(_User, _Peer) -> shell:start() end},
                         {exec, disabled}, {subsystems, []}]),
    try
        {ok, Info} = ssh:daemon_info(D),
        Port = proplists:get_value(port, Info),
        {ok, C} = ssh:connect({127,0,0,1}, Port,
                              [{user, "beam_test"}, {password, Password},
                               {silently_accept_hosts, true}, {save_accepted_host, false},
                               {user_interaction, false}, {user_dir, Tmp}], 5000),
        try
            {ok, Ch} = ssh_connection:session_channel(C, 5000),
            success = ssh_connection:ptty_alloc(C, Ch, [], 5000),
            ok = ssh_connection:shell(C, Ch),
            ok = ssh_connection:send(C, Ch, "io:format(\"~s~n\", [\"OTP29_\" ++ \"SHELL_OK\"]).\n"),
            shell_output(C, Ch, <<>>),
            {ok, ExecCh} = ssh_connection:session_channel(C, 5000),
            success = ssh_connection:exec(C, ExecCh, "1+1.", 5000),
            receive {ssh_cm, C, {exit_status, ExecCh, Status}} when Status /= 0 -> ok
            after 5000 -> error(exec_was_not_disabled) end,
            {ok, SftpCh} = ssh_connection:session_channel(C, 5000),
            failure = ssh_connection:subsystem(C, SftpCh, "sftp", 5000),
            io:format("PASS authenticated SSH shell evaluated Erlang; exec and SFTP rejected~n")
        after ssh:close(C) end
    after ssh:stop_daemon(D) end.
shell_output(C, Ch, Acc) ->
    receive
        {ssh_cm, C, {data, Ch, _, Data}} ->
            All = <<Acc/binary, Data/binary>>,
            case binary:match(All, <<"OTP29_SHELL_OK">>) of
                nomatch -> shell_output(C, Ch, All);
                _ -> ok
            end
    after 10000 -> error(ssh_shell_timeout) end.
