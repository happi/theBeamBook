-module(dirty_nif).
-export([init/0, heavy_work/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./dirty_nif", 0).

heavy_work(_Seconds) ->
    erlang:nif_error(nif_not_loaded).
