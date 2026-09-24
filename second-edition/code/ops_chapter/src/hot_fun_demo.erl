-module(hot_fun_demo).
-export([funs/0, foo/1]).

-ifndef(INCREMENT).
-define(INCREMENT, 1).
-endif.

funs() ->
    F = fun ?MODULE:foo/1,
    L = fun(X) -> foo(X) end,
    {F, L}.

foo(X) -> X + ?INCREMENT.
