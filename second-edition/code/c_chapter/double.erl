-module(double).
-export([start/0, stop/0, double/1]).

start() ->
    SharedLib = "double_drv",
    case erl_ddll:load_driver(".", SharedLib) of
        ok -> ok;
        {error, already_loaded} -> ok;
        _ -> exit({error, could_not_load_driver})
    end,
    register(double_server, spawn(fun() -> init(SharedLib) end)).

init(SharedLib) ->
    Port = open_port({spawn, SharedLib}, []),
    loop(Port).

loop(Port) ->
    receive
        {double, Caller, N} ->
            Port ! {self(), {command, integer_to_list(N)}},
            receive
                {Port, {data, Result}} ->
                    Reply = case Result of
                                "error" -> {error, badarg};
                                _ -> list_to_integer(Result)
                            end,
                    Caller ! {double_result, Reply}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} -> exit(normal)
            end
    end.

double(N) when is_integer(N) ->
    double_server ! {double, self(), N},
    receive
        {double_result, Result} -> Result
    end;
double(_) ->
    {error, badarg}.

stop() ->
    double_server ! stop.
