#!/usr/bin/env escript
%%! +S 2 -sname book_a -setcookie beam_book_demo
-mode(compile).
main(_) ->
    [] = nodes(),
    {ok, Peer, B} = peer:start_link(#{name => book_b, connection => standard_io,
        args => ["+S", "2", "-setcookie", "beam_book_demo"]}),
    try
        [] = nodes(),
        io:format("OTP: ~s; ERTS: ~s~n", [erlang:system_info(otp_release), erlang:system_info(version)]),
        io:format("EPMD: ~p~n", [erl_epmd:names()]),
        [_Name, Host] = string:split(atom_to_list(B), "@"),
        io:format("lookup: ~p~n", [erl_epmd:port_please("book_b", Host)]),
        Parent = self(),
        {ok, _} = dbg:tracer(process, {fun({trace, _, call, {dist_util, F, _}}, S) -> S ! {handshake, F}, S; (_, S) -> S end, Parent}),
        {ok, _} = dbg:p(all, c),
        Fs = [send_name, recv_status, recv_challenge, send_challenge_reply, recv_challenge_ack],
        [{ok, _} = dbg:tpl(dist_util, F, []) || F <- Fs],
        true = net_kernel:connect_node(B),
        Seen = collect(5, []),
        dbg:stop(),
        io:format("handshake: ~p~n", [Seen]),
        Fs = Seen,
        B = rpc:call(B, erlang, node, []),
        io:format("RPC: ~p~n", [B]),
        true = monitor_node(B, true),
        peer:stop(Peer),
        receive {nodedown, B} -> io:format("nodedown: ~p~n", [B]) after 5000 -> error(missing_nodedown) end,
        [] = nodes(),
        {badrpc, nodedown} = rpc:call(B, erlang, node, [], 1000),
        true = monitor_node(B, false),
        io:format("PASS: discovery, handshake, RPC, disconnect, failed RPC~n")
    after
        try dbg:stop() catch _:_ -> ok end,
        try peer:stop(Peer) catch _:_ -> ok end
    end.
collect(0, Acc) -> lists:reverse(Acc);
collect(N, Acc) ->
    receive {handshake, F} -> collect(N - 1, [F|Acc])
    after 5000 -> error({missing_handshake_trace, lists:reverse(Acc)}) end.
