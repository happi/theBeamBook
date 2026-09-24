-module(fun_walk).
-export([make/2]).

make(X, Y) -> fun() -> {X, Y} end.
