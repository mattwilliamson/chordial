%%%-------------------------------------------------------------------
%%% @author Matt Williamson <dawsdesign@gmail.com>
%%% @copyright (C) 2009, Matt Williamson
%%% @doc This module provides a chord server as well as callbacks so
%%% the client using chord can be notified of events, such as a node
%%% joining or leaving the ring.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gen_chord).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% Behaviour
-export([behaviour_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {key=nil,bootstrap_hosts=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the node as a standalone node
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() -> start_link([]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the node and attempts to place itself in the correct
%% location on the ring according to the known nodes.
%%
%% @spec start_link(KnownNodes) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(KnownHosts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, KnownHosts, []).
    
%%%===================================================================
%%% Behaviour
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Get list of callbacks for the chord behaviour.
%% @spec behaviour_info(callbacks) -> [{CallbackFun, Arity}]
%% where
%% CallbackFun = atom(),
%% Arity = integer()
%% @end
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{new_predecessor, 1}, {new_successor, 1}].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(KnownHosts) ->
	NodeName = atom_to_list(node()),
	NodeKey = chord_lib:hash(NodeName),
	chord_lib:init_tables(NodeKey),
    {ok, #state{key=NodeKey, bootstrap_hosts=KnownHosts}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

% Simple ping call
handle_call(ping, _From, State) ->
    Reply = pong,
    {reply, Reply, State};
    
% Get the current state of the server
handle_call(state, _From, State) ->
    {reply, State, State};
    
% Get the current state of the server
handle_call({finger, all}, _From, State) ->
	Reply = ets:all(),
    {reply, Reply, State};

% Unkown Call
handle_call(_Request, _From, State) ->
    Reply = {error, unknown_call},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
