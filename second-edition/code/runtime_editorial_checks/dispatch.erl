-module(dispatch).
-export([run/2]).

run(X, Y) -> collect(X, X + Y, X, 42).

collect(A, B, C, D) -> {A, B, C, D}.
