-module(poa_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    poa:start_link(1234, []).

stop(_State) ->
    ok.
