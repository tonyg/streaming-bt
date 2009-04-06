-module(poa).

-behaviour(gen_server).

-export([start/0, stop/0]).

-export([start_link/2]).
-export([inject/2, subscribe/3, unsubscribe/2]).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("poa.hrl").

start() ->
    application:start(poa).

stop() ->
    application:stop(poa).

start_link(LocalNames, RemoteNames) ->
    gen_server:start_link(?MODULE, [LocalNames, RemoteNames], []).

inject(PoaPid, Message) ->
    gen_server:cast(PoaPid, {inject, Message}).

subscribe(PoaPid, SubscriberKey, CallbackFun) ->
    gen_server:call(PoaPid, {subscribe, SubscriberKey, CallbackFun}).

unsubscribe(PoaPid, SubscriberKey) ->
    gen_server:call(PoaPid, {unsubscribe, SubscriberKey}).

%%---------------------------------------------------------------------------

-record(state, {node_id, %% node_id()
                tcp_listener_pids, %% pid()
                established_links, %% dict(node_id(), pid())
                announce_queue, %% [pid()]
                index, %% dict(node_id(), {intervals(msgid()), dict(msgid(), msg())})
                next_msgid, %% msgid()
                subscribers, %% dict(subscriber_key(), callback_fun())
                node_directory %% dict(node_id(), [{tcp_address(), tcp_port()}])
               }).

start_outbound(RemoteNames) ->
    gen_server:start(poa_outbound, [RemoteNames, self()], []).

ensure_outbound(NodeId, TotalNames, EstablishedLinks) ->
    case dict:find(NodeId, EstablishedLinks) of
        {ok, LinkPid} ->
            gen_server:cast(LinkPid, {replace_addresses, TotalNames});
        error ->
            start_outbound(TotalNames)
    end.

merge_directory([], UpdatedDirectory, _NodeId, _EstablishedLinks) ->
    UpdatedDirectory;
merge_directory([{OwnNodeId, _NodeNames} | Rest], UpdatedDirectory, OwnNodeId, EstablishedLinks) ->
    merge_directory(Rest, UpdatedDirectory, OwnNodeId, EstablishedLinks);
merge_directory([{NodeId, NodeNames} | Rest], UpdatedDirectory, OwnNodeId, EstablishedLinks) ->
    NewNames = case dict:find(NodeId, UpdatedDirectory) of
                   {ok, ExistingNodeNames} ->
                       sets:to_list(sets:union(sets:from_list(NodeNames),
                                               sets:from_list(ExistingNodeNames)));
                   error ->
                       NodeNames
               end,
    ensure_outbound(NodeId, NewNames, EstablishedLinks),
    merge_directory(Rest,
                    dict:store(NodeId, NewNames, UpdatedDirectory),
                    OwnNodeId,
                    EstablishedLinks).

broadcast_state(State = #state{announce_queue = [],
                               established_links = EstablishedLinks}) ->
    case [Pid || {_NodeId, Pid} <- dict:to_list(EstablishedLinks)] of
        [] ->
            State;
        LinkPids ->
            broadcast_state(State#state{announce_queue = LinkPids})
    end;
broadcast_state(State = #state{announce_queue = [LinkPid | Rest]}) ->
    announce_node_info(LinkPid, State),
    announce_index(LinkPid, State),
    State#state{announce_queue = Rest}.

send_to_subscribers(Messages, Subs) ->
    error_logger:info_report({?MODULE, send_to_subscribers, Messages}),
    dict:fold(fun (SubscriberKey, CallbackFun, ok) ->
                      catch CallbackFun(SubscriberKey, Messages),
                      ok
              end, ok, Subs).

summarise_index(NodeId, Index) ->
    error_logger:info_report({?MODULE, summarise_index, NodeId,
                              dict:fold(fun (N, {SeenSet, _NodeIndex}, Acc) ->
                                                [{N, SeenSet} | Acc]
                                        end, [], Index)}),
    ok.

inject_message(NodeId, MsgId, Message, State = #state{index = OldIndex, subscribers = Subs}) ->
    {SeenSet, NodeIndex} = node_lookup(NodeId, OldIndex),
    ok = send_to_subscribers([Message], Subs),
    NewIndex = dict:store(NodeId,
                          {intervals:union(SeenSet,
                                           intervals:single_int(MsgId)),
                           dict:store(MsgId, Message, NodeIndex)},
                          OldIndex),
    summarise_index(State#state.node_id, NewIndex),
    State#state{index = NewIndex}.

node_lookup(NodeId, Index) ->
    case dict:find(NodeId, Index) of
        {ok, V} ->
            V;
        error ->
            {intervals:empty(), dict:new()}
    end.

send_term(LinkPid, Term) ->
    gen_server:cast(LinkPid, {send_term, Term}),
    ok.

announce_node_info(LinkPid, #state{node_id = NodeId,
                                   node_directory = NodeDirectory}) ->
    send_term(LinkPid, {node_info, NodeId, NodeDirectory}).

announce_index(LinkPid, #state{node_id = NodeId,
                               index = Index}) ->
    Summary = dict:fold(fun (Id, {SeenSet, _NodeIndex}, Acc) ->
                                [{Id, SeenSet} | Acc]
                        end, [], Index),
    send_term(LinkPid, {node_index, NodeId, Summary}).

perform_gets([], _LinkPid, _Index) ->
    ok;
perform_gets([{NodeId, RemoteSeenSet} | Rest], LinkPid, Index) ->
    {LocalSeenSet, _LocalNodeIndex} = node_lookup(NodeId, Index),
    UnseenSet = intervals:difference(RemoteSeenSet, LocalSeenSet),
    case intervals:is_empty(UnseenSet) of
        false ->
            send_term(LinkPid, {get_messages, NodeId, UnseenSet});
        true ->
            ok
    end,
    perform_gets(Rest, LinkPid, Index).

%%---------------------------------------------------------------------------

init([LocalNames, RemoteNames]) ->
    NodeId =
        poa_util:hexify(crypto:sha(term_to_binary({os:cmd("uname -a"), node(), erlang:now()}))),
    TcpListenerPids =
        [TcpListenerPid
         || {ok, TcpListenerPid} <- [generic_tcp_server:start_link(poa_inbound, Addr, Port,
                                                                   [binary, {packet, 4},
                                                                    {reuseaddr, true}],
                                                                   [self()])
                                     || {Addr, Port} <- LocalNames]],
    process_flag(trap_exit, true),
    lists:foreach(fun (RemoteName) ->
                          start_outbound([RemoteName])
                  end, RemoteNames),
    {ok, _TRef} = timer:send_interval(?STATE_BROADCAST_INTERVAL_MS, broadcast_state),
    {ok, #state{node_id = NodeId,
                tcp_listener_pids = TcpListenerPids,
                established_links = dict:new(),
                announce_queue = [],
                index = dict:new(),
                next_msgid = 0,
                subscribers = dict:new(),
                node_directory = dict:store(NodeId, LocalNames, dict:new())}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({subscribe, SubscriberKey, CallbackFun}, _From,
            State = #state{subscribers = Subs}) ->
    {reply, ok, State#state{subscribers = dict:store(SubscriberKey, CallbackFun, Subs)}};
handle_call({unsubscribe, SubscriberKey}, _From,
            State = #state{subscribers = Subs}) ->
    {reply, ok, State#state{subscribers = dict:erase(SubscriberKey, Subs)}};
handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast({inject, Message}, State = #state{node_id = NodeId,
                                              next_msgid = MsgId}) ->
    {noreply, broadcast_state(inject_message(NodeId, MsgId, Message,
                                             State#state{next_msgid = MsgId + 1}))};
handle_cast({announce_node_info, LinkPid}, State) ->
    announce_node_info(LinkPid, State),
    {noreply, State};
handle_cast({link_up, RemoteNodeId, LinkPid, RemoteDirectory},
            State = #state{node_id = NodeId,
                           node_directory = LocalDirectory,
                           established_links = EstablishedLinks}) ->
    NewEstablishedLinks = dict:store(RemoteNodeId, LinkPid, EstablishedLinks),
    NewDirectory = merge_directory(dict:to_list(RemoteDirectory),
                                   LocalDirectory,
                                   NodeId,
                                   NewEstablishedLinks),
    %%error_logger:info_report({?MODULE, link_up, {NodeId, LinkPid, RemoteNodeId},
    %%                          dict:to_list(NewDirectory)}),
    {noreply, State#state{node_directory = NewDirectory,
                          established_links = NewEstablishedLinks}};
handle_cast({index_received, _RemoteNodeId, RemoteIndexSummary, LinkPid},
            State = #state{index = OldIndex}) ->
    ok = perform_gets(RemoteIndexSummary, LinkPid, OldIndex),
    {noreply, State};
handle_cast({get_messages, NodeId, UnseenSet, LinkPid},
            State = #state{index = Index}) ->
    {_SeenSet, NodeIndex} = node_lookup(NodeId, Index),
    ok = intervals:foldl_int(fun (MsgId, ok) ->
                                     case dict:find(MsgId, NodeIndex) of
                                         {ok, Message} ->
                                             send_term(LinkPid, {message, NodeId, MsgId, Message});
                                         error ->
                                             ok
                                     end
                             end, ok, UnseenSet),
    {noreply, State};
handle_cast({message, NodeId, MsgId, Message}, State) ->
    {noreply, inject_message(NodeId, MsgId, Message, State)};
handle_cast(Request, State) ->
    {stop, {unhandled_cast, Request}, State}.

handle_info(broadcast_state, State) ->
    {noreply, broadcast_state(State)};
handle_info(Message, State) ->
    {stop, {unhandled_info, Message}, State}.
