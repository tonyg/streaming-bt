-module(poa_test).

-export([injector/2, extractor/2]).

injector(MyPort, OtherPort) ->
    {ok, spawn_link(fun () ->
                            {ok, Poa} =
                                poa:start_link([{{127,0,0,1},MyPort}], [{{127,0,0,1},OtherPort}]),
                            injector_main(Poa, 0)
                    end)}.

injector_main(Poa, N) ->
    poa:inject(Poa, {msg, N}),
    timer:sleep(1000),
    injector_main(Poa, N + 1).

extractor(MyPort, OtherPort) ->
    {ok, Poa} = poa:start_link([{{127,0,0,1},MyPort}], [{{127,0,0,1},OtherPort}]),
    poa:subscribe(Poa, extractor_key, fun (extractor_key, Messages) ->
                                              error_logger:info_report({?MODULE, extractor,
                                                                        Messages})
                                      end),
    {ok, Poa}.
