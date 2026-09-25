-module(pg_io_server).
-export([start_link/0, init/0, loop/1, handle_request/2]).

-record(state, {conn}).

start_link() ->
    spawn_link(?MODULE, init, []).

init() ->
    case epgsql:connect(#{
        host => "127.0.0.1",
        username => "postgres",
        password => "",  % Default user usually does not need a password for local connections
        database => "mydb"
    }) of
        {ok, Conn} ->
            io:format("Successfully connected to PostgreSQL~n"),
            ?MODULE:loop(#state{conn = Conn});
        {error, Reason} ->
            io:format("Failed to connect to PostgreSQL: ~p~n", [Reason]),
            exit(Reason)
    end.

loop(State) ->
    receive
        {io_request, From, ReplyAs, Request} ->
            case handle_request(Request, State) of
                {ok, Reply, NewState} ->
                    From ! {io_reply, ReplyAs, Reply},
                    loop(NewState);
                {stop, Reply, NewState} ->
                    From ! {io_reply, ReplyAs, Reply},
                    epgsql:close(NewState#state.conn),
                    exit(normal)
            end;
        _Unknown ->
            loop(State)
    end.

handle_request({put_chars, _Encoding, Chars}, #state{conn = Conn} = State) ->
    Query = "INSERT INTO io_data (content) VALUES ($1)",
    {ok, _} = epgsql:squery(Conn, Query, [Chars]),
    {ok, ok, State};
handle_request({get_chars, _Encoding, _Prompt, N}, #state{conn = Conn} = State) ->
    Query = "SELECT content FROM io_data LIMIT $1",
    {ok, Result} = epgsql:squery(Conn, Query, [N]),
    {ok, [Row || {_, Row} <- epgsql:decode(Result)], State};
handle_request({get_line, _Encoding, _Prompt}, #state{conn = Conn} = State) ->
    Query = "SELECT content FROM io_data LIMIT 1",
    {ok, Result} = epgsql:squery(Conn, Query, []),
    {ok, [Row || {_, Row} <- epgsql:decode(Result)], State};
handle_request(getopts, State) ->
    {ok, [], State};
handle_request({setopts, _Opts}, State) ->
    {ok, ok, State};
handle_request(_Other, State) ->
    {error, {error, request}, State}.

