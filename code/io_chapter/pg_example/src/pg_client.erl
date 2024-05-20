-module(pg_client).
-export([start/0, put_chars/1, get_chars/1, get_line/0]).

start() ->
    {ok, Pid} = pg_io_server:start_link(),
    register(pg_io_server, Pid).

put_chars(Data) ->
    pg_io_server ! {io_request, self(), pg_io_server, {put_chars, latin1, Data}},
    receive
        {io_reply, pg_io_server, Reply} ->
            Reply
    end.

get_chars(N) ->
    pg_io_server ! {io_request, self(), pg_io_server, {get_chars, latin1, '', N}},
    receive
        {io_reply, pg_io_server, Reply} ->
            Reply
    end.

get_line() ->
    pg_io_server ! {io_request, self(), pg_io_server, {get_line, latin1, ''}},
    receive
        {io_reply, pg_io_server, Reply} ->
            Reply
    end.
