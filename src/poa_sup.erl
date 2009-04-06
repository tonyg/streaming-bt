-module(poa_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10},
          [
           {poa_1,
            {poa, start_link, [1234, []]},
            transient, 5000, worker, dynamic},
           {poa_2,
            {poa, start_link, [2468, [{{127,0,0,1},1234}]]},
            transient, 5000, worker, dynamic}
           ]}}.
