%%%-------------------------------------------------------------------
%% @doc pg_example public API
%% @end
%%%-------------------------------------------------------------------

-module(pg_example_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pg_example_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
