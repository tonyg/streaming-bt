-module(poa_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    poa_sup:start_link().

stop(_State) ->
    ok.
