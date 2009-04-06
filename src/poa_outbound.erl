-module(poa_outbound).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("poa.hrl").

%%---------------------------------------------------------------------------

attempt_connection([], State = #poa_link_state{remote_addresses = RemoteNames}) ->
    error_logger:warning_report({?MODULE, self(), all_attempts_failed, RemoteNames}),
    {ok, _TRef} = timer:send_after(?OUTBOUND_CONNECTION_RETRY_DELAY_MS,
                                   self(), check_connection),
    State;
attempt_connection([RemoteName = {RemoteAddress, RemotePort} | Rest], State) ->
    case gen_tcp:connect(RemoteAddress, RemotePort, [binary, {packet, 4}]) of
        {ok, Sock} ->
            poa_link:link_up(State#poa_link_state{sock = Sock});
        {error, Reason} ->
            error_logger:warning_report({?MODULE, connection_attempt_failed,
                                         RemoteName, Reason}),
            attempt_connection(Rest, State)
    end.

init([RemoteNames, PoaPid]) ->
    self() ! check_connection,
    {ok, #poa_link_state{sock = disconnected,
                         poa_pid = PoaPid,
                         remote_addresses = RemoteNames,
                         remote_node_id = unknown}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast(Request, State) ->
    poa_link:handle_cast(Request, State).

handle_info(check_connection, State = #poa_link_state{sock = Sock,
                                                      remote_addresses = RemoteNames}) ->
    case Sock of
        disconnected ->
            {noreply, attempt_connection(RemoteNames, State)};
        _Other ->
            {noreply, State}
    end;
handle_info({tcp, _Sock, Packet}, State) ->
    {noreply, poa_link:handle_packet(Packet, State)};
handle_info({tcp_closed, _Sock}, State) ->
    {stop, normal, State};
handle_info(Message, State) ->
    {stop, {unhandled_info, Message}, State}.
