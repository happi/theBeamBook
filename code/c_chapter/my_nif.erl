-module(my_nif).
-export([add/2, load/0]).

-on_load(load/0).

load() ->
    erlang:load_nif("./my_nif", 0).

add(_A, _B) ->
    erlang:nif_error("NIF not loaded").