-module(msg).

-export([send_on_heap/0
        ,send_off_heap/0]).

send_on_heap() -> send(on_heap).
send_off_heap() -> send(off_heap).

send(How) ->
  %% Spawn a function that loops for a while
  P2 = spawn(fun () -> receiver(How) end),
  %% spawn a sending process
  P1 = spawn(fun () -> sender(P2) end),
  P1.

sender(P2) ->
  %% Send a message that ends up on the heap
  %%  {_,S} = erlang:process_info(P2, heap_size),
  M = loop(0),
  P2 ! self(),
  receive ready -> ok end,
  P2 ! M,
  io:format("~p~n",[P2]),
  ok.

receiver(How) ->
  erlang:process_flag(message_queue_data,How),
  receive P -> P ! ready end,
  %%  loop(100000),
  receive x -> ok end,
  P.


loop(0) -> [done];
loop(N) -> [loop(N-1)].
