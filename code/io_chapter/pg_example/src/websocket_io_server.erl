-module(websocket_io_server).
-export([start_link/1, init/1, handle_info/2, handle_call/3, handle_cast/2]).

start_link(WebSocketPid) ->
    gen_server:start_link(?MODULE, WebSocketPid, []).

init(WebSocketPid) ->
    {ok, WebSocketPid}.

handle_info({io_request, From, ReplyAs, {put_chars, _Encoding, Chars}}, WebSocketPid) ->
    WebSocketPid ! {text, Chars},
    From ! {io_reply, ReplyAs, ok},
    {noreply, WebSocketPid};
handle_info({io_request, _From, _ReplyAs, {get_chars, _Encoding, _Prompt, _N}}, WebSocketPid) ->
    %% Handle reading from WebSocket if necessary
    {noreply, WebSocketPid}.

handle_call(_, _, WebSocketPid) ->
    {reply, ok, WebSocketPid}.

handle_cast(_, WebSocketPid) ->
    {noreply, WebSocketPid}.
