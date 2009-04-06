-module(poa_outbound).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("poa.hrl").

%%---------------------------------------------------------------------------

init([{RemoteAddress, RemotePort}, PoaPid]) ->
    {ok, Sock} = gen_tcp:connect(RemoteAddress, RemotePort, [binary, {packet, 4}]),
    {ok, #poa_link_state{sock = Sock,
                         poa_pid = PoaPid}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast(Request, State) ->
    {stop, {unhandled_cast, Request}, State}.

handle_info(Message, State) ->
    {stop, {unhandled_info, Message}, State}.
