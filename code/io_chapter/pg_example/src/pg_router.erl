-module(pg_router).
-export([start/0, stop/0, dispatch/0]).

start() ->
    Dispatch = dispatch(),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}).

stop() ->
    cowboy:stop_listener(http_listener).

dispatch() ->
    [
        {"/websocket", websocket_handler, []},
        {"/[...]", cowboy_static, {priv_dir, my_app, "static"}}
    ].
