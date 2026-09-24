%% Derived from https://www.erlang.org/doc/system/c_portdriver.html

-module(busy_port).
-export([start/1, stop/0, init/1]).
-export([foo/1, bar/1, async_foo/1, async_bar/1, async_receive/0]).
-export([test_sync_foo/0,
         test_async_foo/0,
         test_async_bar/0]).

start(SharedLib) ->
    case erl_ddll:load_driver(".", SharedLib) of
	    ok -> ok;
	    {error, already_loaded} -> ok;
	    _ -> exit({error, could_not_load_driver})
    end,
    spawn(?MODULE, init, [SharedLib]).

init(SharedLib) ->
    register(busy_port_example, self()),
    Port = open_port({spawn, SharedLib}, []),
    loop(Port, [], 0).

test_sync_foo() ->
    [foo(N) || N <- lists:seq(1, 10)].

test_async_foo() ->
    [async_receive() || _ <- [async_foo(N) || N <- lists:seq(1, 10)]].

test_async_bar() ->
    [async_receive() || _ <- [async_bar(N) || N <- lists:seq(1, 10)]].



stop() ->
    busy_port_example ! stop.

foo(X) ->
    call_port({foo, X}).
bar(Y) ->
    call_port({bar, Y}).

async_foo(X) ->
    send_message({foo, X}).
async_bar(Y) ->
    send_message({bar, Y}).

async_receive() ->
    receive
        {busy_port_example, Data} ->
            Data
    after 2000 -> timeout
    end.

call_port(Msg) ->
    busy_port_example ! {call, self(), Msg},
    receive
	{busy_port_example, Result} ->
	    Result
    end.

send_message(Message) ->
    busy_port_example ! {send, self(), Message}.


reply(Id, [{Id, From}|Ids], Data) ->
    From ! {busy_port_example, Data},
    Ids;
reply(Id, [Id1|Ids], Data) ->
    [Id1 | reply(Id, Ids, Data)];
reply(_Id, [], Data) -> %% oops, no id found
    io:format("No ID found for data: ~p~n", [Data]),
    [].


loop(Port, Ids, Id) ->
    receive
	{call, Caller, Msg} ->
        io:format("Call: ~p~n", [Msg]),
	    Port ! {self(), {command, encode(Msg, Id)}},
	    receive
		{Port, {data, Data}} ->
            Res = decode_data(Data),
            io:format("Received data: ~w~n", [Res]),
		    Caller ! {busy_port_example, Res}
	    end,
	    loop(Port, Ids, Id);
     {Port, {data, Data}} ->
            {Ref, Res} = decode(Data),
            io:format("Received data: ~w~n", [Res]),
            NewIds = reply(Ref, Ids, Res),
            loop(Port, NewIds, Id);
     {send, From, Message} ->
            T1 = os:system_time(millisecond),
            io:format("Send: ~p~n", [Message]),
            Port ! {self(), {command, encode(Message, Id)}},
            T2 = os:system_time(millisecond),
            if (T2 - T1) > 500 -> io:format("Shouldn't ! be async...~n", []);
               true -> ok
            end,
            loop(Port, [{Id, From} | Ids], Id + 1);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    io:format("~p ~n", [Reason]),
	    exit(port_terminated)
    end.

encode({foo, X}, Id) -> [1, X, Id];
encode({bar, X}, Id) -> [2, X, Id].

decode([Int, Id]) -> {Id, Int}.
decode_data([Int,_Id]) -> Int.
