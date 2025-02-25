-module(perf_example).
-export([compute/1, eprof/0, tprof_example/1]).

compute([]) ->
    ok;
compute([H|T]) ->
    _F = factorial(H),
    compute(T).

factorial(0) -> 1;
factorial(N) when N > 0 ->
    N * factorial(N - 1).


eprof() ->
    Compute = fun() -> perf_example:compute([10,15,20,25,30]) end,
    eprof:profile(Compute),
    eprof:analyze().

tprof_example(Type) ->
    Compute = fun() -> perf_example:compute([10,15,20,25,30]) end,
    tprof:profile(Compute, #{type => Type}).