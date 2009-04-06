-module(poa_util).

-export([hexify/1]).

hexify(Bin) when is_binary(Bin) ->
    hexify(binary_to_list(Bin));
hexify([]) ->
    "";
hexify([B | Rest]) ->
    [hex_digit((B bsr 4) band 15), hex_digit(B band 15) | hexify(Rest)].

hex_digit(X) when X < 10 ->
    X + $0;
hex_digit(X) ->
    X + $A - 10.
