-module(arith_typed).
-export([sum_pair/1]).

sum_pair({A, B}) when is_integer(A), A >= 0, A =< 999999,
                         is_integer(B), B >= 0, B =< 999999 ->
    add(A, B).

add(X, Y) -> X + Y.
