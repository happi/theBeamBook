-module(gc_example).
-export([example/0, example/1, gen_data/1, collect/1]).

example() ->
    example(<<"Hello">>).

example(Binary) when is_binary(Binary) ->
    T = gen_data(Binary),
    collect(element(1, T)).

gen_data(Binary) ->
    S = binary_to_list(Binary),
    {[S, S], S}.

collect(S) ->
    erlang:garbage_collect(),
    S.
