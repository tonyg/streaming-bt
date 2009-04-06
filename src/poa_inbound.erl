-module(poa_inbound).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("poa.hrl").

%%---------------------------------------------------------------------------

init([Sock, PoaPid]) ->
    process_flag(trap_exit, true),
    {ok, #poa_link_state{sock = Sock,
                         poa_pid = PoaPid,
                         remote_node_id = unknown}}.

terminate(Reason, State) ->
    poa_link:link_down(Reason, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast({socket_control_transferred, Sock}, State = #poa_link_state{sock = Sock}) ->
    {noreply, poa_link:link_up(State)};
handle_cast(Request, State) ->
    poa_link:handle_cast(Request, State).

handle_info({tcp, _Sock, Packet}, State) ->
    {noreply, poa_link:handle_packet(Packet, State)};
handle_info({tcp_closed, _Sock}, State) ->
    {stop, normal, State};
handle_info(Message, State) ->
    {stop, {unhandled_info, Message}, State}.
