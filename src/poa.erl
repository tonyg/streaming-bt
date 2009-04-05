-module(poa).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Port, Roots) ->
    gen_server:start_link(?MODULE, [Port, Roots], []).

%%---------------------------------------------------------------------------

-record(state, {tcp_listener_pid}).

init([Port, Roots]) ->
    {ok, TcpListenerPid} = generic_tcp_server:start_link(poa_inbound, {0,0,0,0}, Port,
                                                         [binary, {packet, 4}],
                                                         [self()]),
    {ok, #state{tcp_listener_pid = TcpListenerPid}}.

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
