-module(operations_SUITE).
-export([all/0, assertion_headers/1]).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
all() -> [assertion_headers].
assertion_headers(_Config) ->
    ?assert(2 > 1),
    ?assertEqual(42, 21 + 21),
    ?assertNotEqual(0, length(all())),
    ?assertMatch({ok, _}, {ok, 42}),
    ?assertNotMatch({error, _}, {ok, 42}),
    ct:log(100, "All documented assertion macros executed", []).
