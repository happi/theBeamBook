-module(more).
-export([known/1]).
known(X) -> F = fun(Y) -> Y + 1 end, F(X).
