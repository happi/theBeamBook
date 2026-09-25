-module(send).
-export([test/0, test/1]).

test() ->
    test(<<"hello">>).

test(Binary) when is_binary(Binary) ->
    P1 = self(),
    {P2, Ref} = spawn_opt(fun() -> p2(P1) end,
                         [monitor, {message_queue_data, off_heap}]),
    Before = p1(P2, Binary),
    receive
        {P2, After} ->
            receive {'DOWN', Ref, process, P2, normal} -> ok end,
            {{sender, Before}, {receiver, After}};
        {'DOWN', Ref, process, P2, Reason} ->
            error({receiver_failed, Reason})
    end.

p2(P1) ->
    receive
        M -> P1 ! {self(), measure(M)}
    end.

p1(P2, Binary) ->
    L = binary_to_list(Binary),
    M = {L, L},
    Before = measure(M),
    P2 ! M,
    Before.

%% {words with sharing, flat words, are the two lists the same object?}
measure(M) ->
    {erts_debug:size(M), erts_debug:flat_size(M),
     erts_debug:same(element(1, M), element(2, M))}.
