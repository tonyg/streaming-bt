-module(poa).

-behaviour(gen_server).

-export([start/0, stop/0]).

-export([start_link/2]).
-export([inject/2, subscribe/3, unsubscribe/2]).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start() ->
    application:start(poa).

stop() ->
    application:stop(poa).

start_link(Port, Roots) ->
    gen_server:start_link(?MODULE, [Port, Roots], []).

inject(PoaPid, Message) ->
    gen_server:cast(PoaPid, {inject, Message}).

subscribe(PoaPid, SubscriberKey, CallbackFun) ->
    gen_server:call(PoaPid, {subscribe, SubscriberKey, CallbackFun}).

unsubscribe(PoaPid, SubscriberKey) ->
    gen_server:call(PoaPid, {unsubscribe, SubscriberKey}).

%%---------------------------------------------------------------------------

-record(state, {tcp_listener_pid,
                established_links,
                known_links,
                node_id
               }).

init([Port, Roots]) ->
    process_flag(trap_exit, true),
    {ok, TcpListenerPid} = generic_tcp_server:start_link(poa_inbound, {0,0,0,0}, Port,
                                                         [binary, {packet, 4}],
                                                         [self()]),
    lists:foreach(fun (RootAddress) ->
                          gen_server:start(poa_outbound, [RootAddress, self()], [])
                  end, Roots),
    {ok, #state{tcp_listener_pid = TcpListenerPid,
                established_links = dict:new(),
                known_links = dict:new(),
                node_id = crypto:sha(term_to_binary({os:cmd("uname -a"), node(), erlang:now()}))}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({subscribe, SubscriberKey, CallbackFun}, _From, State) ->
    {stop, {not_implemented, subscribe}, State}; %% TODO
handle_call({unsubscribe, SubscriberKey}, _From, State) ->
    {stop, {not_implemented, unsubscribe}, State}; %% TODO
handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast({inject, Message}, State) ->
    {stop, {not_implemented, inject}, State}; %% TODO
handle_cast(Request, State) ->
    {stop, {unhandled_cast, Request}, State}.

handle_info(Message, State) ->
    {stop, {unhandled_info, Message}, State}.
