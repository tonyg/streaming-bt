-module(poa_link).

-export([link_up/1, link_down/2]).
-export([handle_cast/2, handle_packet/2]).

-include("poa.hrl").

link_up(State = #poa_link_state{poa_pid = PoaPid}) ->
    gen_server:cast(PoaPid, {announce_node_info, self()}),
    State.

link_down(_Reason, State = #poa_link_state{remote_node_id = unknown}) ->
    State;
link_down(Reason, State = #poa_link_state{remote_node_id = RemoteNodeId,
                                          poa_pid = PoaPid}) ->
    gen_server:cast(PoaPid, {link_down, RemoteNodeId, self(), Reason}),
    State.

send_term(Term, State = #poa_link_state{sock = Sock}) ->
    gen_tcp:send(Sock, term_to_binary(Term)),
    State.

handle_cast({send_term, Term}, State) ->
    {noreply, send_term(Term, State)};
handle_cast({replace_addresses, TotalNames}, State) ->
    {noreply, State#poa_link_state{remote_addresses = TotalNames}};
handle_cast(Request, State) ->
    {stop, {unhandled_cast, Request}, State}.

handle_packet(Packet, State) ->
    handle_term(binary_to_term(Packet), State).

handle_term({node_info, RemoteNodeId, RemoteDirectory},
            State = #poa_link_state{poa_pid = PoaPid}) ->
    gen_server:cast(PoaPid, {link_up, RemoteNodeId, self(), RemoteDirectory}),
    State#poa_link_state{remote_node_id = RemoteNodeId};
handle_term({node_index, RemoteNodeId, RemoteIndexSummary},
            State = #poa_link_state{poa_pid = PoaPid}) ->
    gen_server:cast(PoaPid, {index_received, RemoteNodeId, RemoteIndexSummary, self()}),
    State;
handle_term({get_messages, NodeId, UnseenSet},
            State = #poa_link_state{poa_pid = PoaPid}) ->
    gen_server:cast(PoaPid, {get_messages, NodeId, UnseenSet, self()}),
    State;
handle_term({message, NodeId, MsgId, Message},
            State = #poa_link_state{poa_pid = PoaPid}) ->
    gen_server:cast(PoaPid, {message, NodeId, MsgId, Message}),
    State;
handle_term(UnknownPacket, State) ->
    exit({unknown_packet, UnknownPacket, State}).
