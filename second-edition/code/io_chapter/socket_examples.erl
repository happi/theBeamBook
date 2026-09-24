-module(socket_examples).
-export([tcp/0, tcp/1, udp/0]).

tcp() -> tcp(inet).
tcp(Backend) ->
    {ok, Listen} = gen_tcp:listen(0, [{inet_backend, Backend}, binary,
                                    {ip, {127,0,0,1}}, {active, false},
                                    {packet, 4}]),
    try
        {ok, {_, Port}} = inet:sockname(Listen),
        {ok, Client} = gen_tcp:connect({127,0,0,1}, Port,
                                      [{inet_backend, Backend}, binary,
                                       {active, false}, {packet, 4}], 1000),
        try
            {ok, Server} = gen_tcp:accept(Listen, 1000),
            try
                ok = gen_tcp:send(Client, <<"ping">>),
                {ok, <<"ping">>} = gen_tcp:recv(Server, 0, 1000),
                ok = gen_tcp:send(Server, <<"pong">>),
                gen_tcp:recv(Client, 0, 1000)
            after gen_tcp:close(Server)
            end
        after gen_tcp:close(Client)
        end
    after gen_tcp:close(Listen)
    end.

udp() ->
    {ok, Socket} = gen_udp:open(0, [binary, {ip, {127,0,0,1}}, {active, false}]),
    try
        {ok, {_, Port}} = inet:sockname(Socket),
        ok = gen_udp:send(Socket, {127,0,0,1}, Port, <<"hello">>),
        {ok, {{127,0,0,1}, Port, Data}} = gen_udp:recv(Socket, 0, 1000),
        Data
    after gen_udp:close(Socket)
    end.
