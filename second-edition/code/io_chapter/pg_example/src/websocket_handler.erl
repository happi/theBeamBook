-module(websocket_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, _Opts) ->
    {cowboy_websocket, Req, #{}}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    io:format("Received: ~s~n", [Msg]),
    {reply, {text, io_lib:format("Echo: ~s", [Msg])}, State}.

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.
