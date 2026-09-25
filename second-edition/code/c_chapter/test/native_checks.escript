#!/usr/bin/env escript
%% Run from the directory holding the compiled beams and shared libraries.
main(_) ->
    true = code:add_patha("."),
    5 = my_nif:add(2, 3),
    2147483647 = my_nif:add(2147483647, 0),
    -2147483648 = my_nif:add(-2147483648, 0),
    -1 = my_nif:add(2147483647, -2147483648),
    lists:foreach(fun({A,B}) -> badarg(fun() -> my_nif:add(A,B) end) end,
                  [{2147483647,1},{-2147483648,-1},{2147483648,0},
                   {0,-2147483649},{1.0,2},{atom,2}]),
    true = double:start(),
    42 = double:double(21),
    2147483646 = double:double(1073741823),
    -2147483648 = double:double(-1073741824),
    lists:foreach(fun(N) -> {error,badarg} = double:double(N) end,
                  [1073741824,-1073741825,1 bsl 100,atom]),
    ok = erl_ddll:load_driver(".", "double_drv"),
    P = open_port({spawn_driver,"double_drv"}, [binary]),
    lists:foreach(fun(B) ->
        true = port_command(P,B),
        receive {P,{data,<<"error">>}} -> ok after 1000 -> error(timeout) end
    end, [<<>>,<<"1",0,"x">>,<<"-">>,<<"1x">>,<<"+1">>]),
    true = port_close(P),
    double:stop(),
    io:format("Native bounds and malformed-input checks passed.~n").
badarg(F) ->
    try F() of Value -> error({expected_badarg,Value})
    catch error:badarg -> ok end.
