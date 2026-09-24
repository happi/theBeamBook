-module(custom_io_server).
-export([start_link/0, stop/1, init/0, loop/1, handle_request/2]).

-record(state, {buffer = [], pos = 0}).

start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

init() -> loop(#state{}).

stop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {io_request, self(), Ref, stop},
    receive
        {io_reply, Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} -> {error, Reason}
    end.

loop(State) ->
    receive
        {io_request, From, ReplyAs, Request} when is_pid(From) ->
            case handle_request(Request, State) of
                {stop, Reply, _} ->
                    From ! {io_reply, ReplyAs, Reply};
                {ok, Reply, Next} ->
                    From ! {io_reply, ReplyAs, Reply},
                    loop(Next);
                {error, Reason, Next} ->
                    From ! {io_reply, ReplyAs, {error, Reason}},
                    loop(Next)
            end
    end.

handle_request({put_chars, Encoding, Chars}, State)
  when Encoding =:= latin1; Encoding =:= unicode ->
    case characters(Chars, Encoding) of
        {ok, List} ->
            {ok, ok, State#state{buffer = State#state.buffer ++ List}};
        error -> {error, put_chars, State}
    end;
handle_request({put_chars, Encoding, M, F, Args}, State) ->
    try apply(M, F, Args) of
        Chars -> handle_request({put_chars, Encoding, Chars}, State)
    catch _:_ -> {error, put_chars, State}
    end;
handle_request({get_chars, Encoding, _Prompt, N}, State)
  when is_integer(N), N >= 0 ->
    Rest = lists:nthtail(State#state.pos, State#state.buffer),
    {Part, _} = lists:split(min(N, length(Rest)), Rest),
    read_reply(Part, Encoding, N =/= 0 andalso Rest =:= [], State);
handle_request({get_line, Encoding, _Prompt}, State) ->
    Rest = lists:nthtail(State#state.pos, State#state.buffer),
    {Before, After} = lists:splitwith(fun(C) -> C =/= $\n end, Rest),
    Part = case After of [] -> Before; _ -> Before ++ "\n" end,
    read_reply(Part, Encoding, Rest =:= [], State);
handle_request(getopts, State) ->
    {ok, [{binary, true}, {encoding, unicode}], State};
handle_request({setopts, [{binary, true}]}, State) ->
    {ok, ok, State};
handle_request(stop, State) ->
    {stop, {ok, unicode:characters_to_binary(State#state.buffer)}, State};
handle_request(_, State) ->
    {error, request, State}.

characters(Chars, Encoding) ->
    try unicode:characters_to_list(Chars, Encoding) of
        List when is_list(List) -> {ok, List};
        _ -> error
    catch _:_ -> error
    end.

read_reply(_, Encoding, _, State)
  when Encoding =/= latin1, Encoding =/= unicode ->
    {error, request, State};
read_reply(_, _, true, State) -> {ok, eof, State};
read_reply(Part, Encoding, false, State)
  when Encoding =:= latin1; Encoding =:= unicode ->
    case unicode:characters_to_binary(Part, unicode, Encoding) of
        Binary when is_binary(Binary) ->
            {ok, Binary, State#state{pos = State#state.pos + length(Part)}};
        _ -> {error, no_translation, State}
    end;
read_reply(_, _, _, State) -> {error, request, State}.
