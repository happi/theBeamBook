-module(lb).
-export([start/0]).

start() ->
    Workers = [spawn(fun worker/0) || _ <- lists:seq(1,10)],
    LoadBalancer = spawn(fun() -> loop(Workers, 0) end),
    {ok, Files} = file:list_dir("."),
    Loaders = [spawn(fun() -> loader(LoadBalancer, F) end) || F <- Files],
    {Loaders, LoadBalancer, Workers}.

loader(LB, File) ->
    case  file:read_file(File) of
        {ok, Bin} ->  LB ! Bin;
        _Dir -> ok
    end,
    ok.

worker() ->
    receive
        Bin ->
            io:format("Byte Size: ~w~n", [byte_size(Bin)]),
            garbage_collect(),
            worker()
    end.


loop(Workers, N) ->
  receive
    WorkItem ->
       Worker = lists:nth(N+1, Workers),
       Worker ! WorkItem,
       loop(Workers, (N+1) rem length(Workers))
  end.
