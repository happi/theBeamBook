-module(world).
-export([hello/0]).

-include("world.hrl").

hello() -> ?GREETING.

