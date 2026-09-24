-module(file_client).
-export([open/0, close/1, write/2, read/2, read_line/1]).

open() ->
    {ok, Pid} = custom_io_server:start_link(),
    {ok, Pid}.

close(Device) ->
    custom_io_server:stop(Device).

write(Device, Data) ->
    file:write(Device, Data).

read(Device, Length) ->
    file:read(Device, Length).

read_line(Device) ->
    file:read_line(Device).
