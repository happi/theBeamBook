-module(feature_demo).
-export([value/1]).

-ifdef(USE_MAYBE).
value(Result) ->
    maybe
        {ok, N} ?= Result,
        N + 1
    end.
-else.
value(Result) ->
    case Result of
        {ok, N} -> N + 1;
        Other -> Other
    end.
-endif.
