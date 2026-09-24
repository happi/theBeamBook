-module(newops).
-export([upd/1, body/3, small/1, nr_new/2, nr_get/1, nr_is/1]).
-record(point, {x = 0, y = 0, z = 0}).
-record #vec{x = 0, y = 0}.
upd(P) -> P#point{y = 42}.
body(X, Lo, Hi) -> is_integer(X, Lo, Hi).
small(X) when is_integer(X, 0, 255) -> small;
small(_) -> other.
nr_new(X, Y) -> #vec{x = X, y = Y}.
nr_get(V) -> V#vec.x.
nr_is(#vec{x = X}) -> X;
nr_is(_) -> no.
