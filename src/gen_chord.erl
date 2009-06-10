%%%-------------------------------------------------------------------
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%% @copyright (C) 2009, Matt Williamson
%%% @doc This module provides a chord server as well as callbacks so
%%% the client using chord can be notified of events, such as a node
%%% joining or leaving the ring.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gen_chord).

-include("chordial.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, call/1, call/2, find_successor/1, find_successor/2, lookup/1]).

%% Behaviour
-export([behaviour_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define('MAX_KEY', chord_lib:max_hash_value()).

% TODO: Change successor to list of successors
-record(state, {key, predecessor=nil, finger_table=[], successor=node()}).

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
    
%%--------------------------------------------------------------------
%% @doc
%% Shortcut for gen_server:call(gen_chord, Request)
%%
%% @spec call(Request::term()) -> Reply::term()
%% @end
%%--------------------------------------------------------------------
call(Request) ->
    gen_server:call(?SERVER, Request).
    
%%--------------------------------------------------------------------
%% @doc
%% Shortcut for gen_server:call({gen_chord, node()}, Request)
%%
%% @spec call(Request::term(), node()) -> Reply::term()
%% @end
%%--------------------------------------------------------------------
call(Request, Node) ->
    error_logger:info_msg("Calling ~p with message:~p~n", [Node, Request]),
    net_kernel:connect_node(Node),
    gen_server:call({?SERVER, Node}, Request).
    
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
%% @spec init(KnownNodes::[node()]) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(KnownNodes) when is_atom(KnownNodes) ->
    init([KnownNodes]);
    
init(KnownNodes) ->
    error_logger:info_msg("Starting gen_chord...~n"),
	Key = chord_lib:hash(node()),
	error_logger:info_msg("Starting up node ~p (~p) with known nodes: ~p~n", [node(), Key, KnownNodes]),
	%FingerTable = init_finger_table(NodeKey),
	error_logger:info_msg("Getting successor...~n"),
	Successor = case KnownNodes of
	    [] -> node();
	    [KnownNode|_Rest] -> 
	        {ok, Node} = find_successor(Key, KnownNode),
	        Node
	end,
	error_logger:info_msg("Got successor: ~p~n", [Successor]),
    % TODO: Set predecessor here (successors old predecessor)
	%error_logger:info_msg("Got Predecessor: ~p~n", [Predecessor]),
	notify_successor(Successor),
	error_logger:info_msg("Started gen_chord.~n"),
	spawn_link(fun() -> periodic_stabilize() end),
    {ok, #state{key=Key, 
                %finger_table=FingerTable,
                successor=Successor}}.

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
    
%find_successor
handle_call({find_successor, Key}, _From, State) ->
    error_logger:info_msg("Finding successor to ~p...~n", [Key]),
    CurrentSuccessor = State#state.successor,
    Successor = case successor_is_successor(Key, State) of
        true -> CurrentSuccessor;
        % TODO: Substitute this finde_successor call with a closest_preceeding_node
        % call to abstract out the finger table lookup
        false -> 
            {ok, Node} = find_successor(Key, CurrentSuccessor),
            Node
    end,
    Reply = {ok, Successor},
    {reply, Reply, State};
    
handle_call({new_predecessor, NewPredecessor}, From, State) ->
    NewPredecessorKey = chord_lib:hash(NewPredecessor),
    OldPredecessor = State#state.predecessor,
    OldPredecessorKey = chord_lib:hash(OldPredecessor),
    SuccessorKey = State#state.key,
    IsPredecessor = (OldPredecessor =:= nil) orelse between(OldPredecessorKey, SuccessorKey, NewPredecessorKey, false),
    NewState = case IsPredecessor of
        true -> 
            error_logger:info_msg("New predecessor: ~p~n", [NewPredecessor]), 
            State#state{predecessor=NewPredecessor};
        false -> State
    end,
    SuccessorIsSelf = State#state.successor =:= node(),
    NewState2 = case SuccessorIsSelf of
        true -> 
            gen_server:reply(From, ok),
            error_logger:info_msg("New successor: ~p~n", [NewPredecessor]),
            notify_successor(NewPredecessor),
            NewState#state{successor=NewPredecessor};
        false -> NewState
    end,
    Reply = ok,
    {reply, Reply, NewState2};
    
% Our first successor is down, find the next one and tell it we are its
% predecessor
handle_call({node_down}, _From, State) ->
    Successor = find_successor(State#state.key, State),
    notify_successor(Successor),
    NewState = State#state{successor=Successor},
	Reply = ok,
    {reply, Reply, NewState};
    
handle_call(stabilize, _From, State) ->
    error_logger:info_msg("Stabilizing... State: ~p~n", [State]),
    Successor = State#state.successor,
    SuccessorKey = chord_lib:hash(Successor),
    Predecessor = State#state.predecessor,
    PredecessorKey = chord_lib:hash(Predecessor),
    {ok, SuccessorPredecessor} = case Successor =:= node() of
        true -> {ok, node()};
        _ -> call(predecessor, Successor)
    end,
    error_logger:info_msg("Got SuccessorPredecessor: ~p~n", [SuccessorPredecessor]),
    SuccessorPredecessorKey = chord_lib:hash(SuccessorPredecessor),
    NewState = case (SuccessorPredecessor /= node()) andalso between(PredecessorKey, SuccessorKey, SuccessorPredecessorKey, false) of
        true -> 
            error_logger:info_msg("New successor: ~p~n", SuccessorPredecessor),
            notify_successor(SuccessorPredecessor),
            State#state{successor=SuccessorPredecessor};
        false -> State
    end,
    error_logger:info_msg("Stabilized state: ~p~n", [NewState]),
	Reply = ok,
    {reply, Reply, NewState};
    
handle_call(fix_fingers, _From, State) ->
    Key = State#state.key,
    Successor = find_successor(key),
    {reply, ok, State};

handle_call(state, _From, State) ->
    {reply, State, State};
    
handle_call(predecessor, _From, State) ->
    Reply = {ok, State#state.predecessor},
    {reply, Reply, State};

% Unkown Call
handle_call(Request, _From, State) ->
    error_logger:warning_msg("Unknown call to module ~p: ~p~n", [?MODULE, Request]),
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
    error_logger:warning_msg("Code change.~n"),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the successor for the node when it starts up.
%% @spec init_successor(KnownNodes::[node()], key()) -> {ok, node()} |
%%                                                      {error, Reason}
%% @end
%%--------------------------------------------------------------------
    
notify_successor(Successor) when is_atom(Successor) ->
    Node = node(),
    case Successor of
        nil -> ok;
        Node -> ok;
        _ -> call({new_predecessor, node()}, Successor)
    end.
    
successor_is_successor(Key, State) ->
    PredecessorKey = State#state.key,
    Successor = State#state.successor,
    SuccessorKey = chord_lib:hash(atom_to_list(Successor)),
    IsSuccessor = Successor =:= node(),
    IsBetween = between(PredecessorKey, SuccessorKey, Key, true),
    IsSuccessor or IsBetween.
    
between(PredecessorKey, SuccessorKey, Key, IsInclusive) ->
    IsLastNode = SuccessorKey =< PredecessorKey,
    Attempt1 = case IsInclusive of
        true -> (PredecessorKey < Key) and (Key =< SuccessorKey);
        false -> (PredecessorKey < Key) and (Key < SuccessorKey)
    end,
    Attempt2 = case IsInclusive of
        true -> IsLastNode and (((PredecessorKey < Key) and (Key =< ?MAX_KEY)) or 
                                ((Key >= 0) and (Key =< SuccessorKey)));
        false -> IsLastNode and (((PredecessorKey < Key) and (Key =< ?MAX_KEY)) or 
                                 ((Key >= 0) and (Key < SuccessorKey)))
    end,
    IsBetween = Attempt1 or Attempt2,
    error_logger:info_msg("Checking if ~p is between ~p and ~p...~p~n", [Key, PredecessorKey, SuccessorKey, IsBetween]),
    IsBetween.
    
lookup(Key) -> find_successor(Key).
    
find_successor(Key) ->
    find_successor(Key, node()).
    
find_successor(Key, Node) ->
    IsInRange = Key >= 0 andalso Key =< ?MAX_KEY,
    case IsInRange of
        true -> call({find_successor, Key}, Node);
        false -> {error, out_of_bounds}
    end.
    
periodic_stabilize() ->
    receive _ -> ok
    after ?stabilize_delay -> ok
    end,
    call(stabilize),
    periodic_stabilize().

%%--------------------------------------------------------------------
%% @private
%% @doc Initialize the tables which store known nodes.
%% @spec init_finger_table(key()) -> [finger_record()]
%% @end
%%--------------------------------------------------------------------



