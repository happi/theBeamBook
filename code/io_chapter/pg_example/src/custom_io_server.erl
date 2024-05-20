-module(custom_io_server).
-export([start_link/0, stop/1, init/0, loop/1, handle_request/2]).

-record(state, {buffer = <<>>, pos = 0}).

start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

init() ->
    ?MODULE:loop(#state{}).

stop(Pid) ->
    Pid ! {io_request, self(), Pid, stop},
    receive
        {io_reply, _, {ok, State}} ->
            {ok, State#state.buffer};
        Other ->
            {error, Other}
    end.

loop(State) ->
    receive
        {io_request, From, ReplyAs, Request} ->
            case handle_request(Request, State) of
                {ok, Reply, NewState} ->
                    From ! {io_reply, ReplyAs, Reply},
                    loop(NewState);
                {stop, Reply, _NewState} ->
                    From ! {io_reply, ReplyAs, Reply},
                    exit(normal);
                {error, Reply, NewState} ->
                    From ! {io_reply, ReplyAs, {error, Reply}},
                    loop(NewState)
            end
    end.

handle_request({put_chars, _Encoding, Chars}, State) ->
    Buffer = State#state.buffer,
    NewBuffer = <<Buffer/binary, Chars/binary>>,
    {ok, ok, State#state{buffer = NewBuffer}};
handle_request({get_chars, _Encoding, _Prompt, N}, State) ->
    Part = binary:part(State#state.buffer, State#state.pos, N),
    {ok, Part, State#state{pos = State#state.pos + N}};
handle_request({get_line, _Encoding, _Prompt}, State) ->
    case binary:split(State#state.buffer, <<$\n>>, [global]) of
        [Line|_Rest] ->
            {ok, <<Line/binary, $\n>>, State};
        _ ->
            {ok, State#state.buffer, State}
    end;
handle_request(getopts, State) ->
    {ok, [], State};
handle_request({setopts, _Opts}, State) ->
    {ok, ok, State};
handle_request(stop, State) ->
    {stop, {ok, State}, State};
handle_request(_Other, State) ->
    {error, {error, request}, State}.