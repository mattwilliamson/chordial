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
-export([start_link/0, start_link/1, call/1, call/2]).

%% Behaviour
-export([behaviour_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define('MAX_KEY', chord_lib:max_hash_value()).

-record(state, {key, predecessor=nil, predecessor_key=nil, finger_table=[], successors=[]}).

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
    net_kernel:connect_node(Node),
    io:format("Connected to ~p...~n", [Node]),
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
	NodeName = atom_to_list(node()),
	NodeKey = chord_lib:hash(NodeName),
	io:format("Starting up node ~p (~p)~n", [NodeName, NodeKey]),
	FingerTable = init_finger_table(NodeKey),
	Successors = case KnownNodes of
	    [] -> [];
	    _ -> 
	        io:format("Getting successor...~n"),
	        [init_successor(KnownNodes, NodeKey)]
	end,
	io:format("Got successors: ~p~n", [Successors]),
	Predecessor = case Successors of
	    [] -> nil;
	    _ -> 
	        io:format("Getting Predecessors...~n"),
	        init_predecessor(hd(Successors))
	end,
	PredecessorKey = case Predecessor of
	    nil -> nil;
	    _ -> chord_lib:hash(atom_to_list(Predecessor))
	end,
	io:format("Got Predecessors: ~p~n", [Predecessor]),
	case Successors /= [] of
	    true -> 
	        io:format("Notifying successor...~n"),
	        call({notify, node(), NodeKey}, hd(Successors));
	    _ -> ok
	end,
    {ok, #state{key=NodeKey, 
                finger_table=FingerTable,
                successors=Successors,
                predecessor=Predecessor,
                predecessor_key=PredecessorKey}}.

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
    
% Get the current finger table
handle_call({finger, all}, _From, State) ->
	Reply = {ok, State#state.finger_table},
    {reply, Reply, State};
    
% Get the closest match for a key from the finger table
handle_call({finger, Key}, _From, State) ->
	Reply = {ok, finger(Key, State#state.finger_table)},
    {reply, Reply, State};
    
% Get the immediate predecessor of this node
handle_call(predecessor, _From, State) ->
	Reply = {ok, State#state.predecessor},
    {reply, Reply, State};
    
% Change this node's predecessor
handle_call({notify, Node, NodeKey}, _From, State) ->
    io:format("New predecessor: ~p (~p)~n", [Node, NodeKey]),
    NewState = State#state{predecessor=Node, predecessor_key=NodeKey},
    NewState2 = case State#state.successors of
        [] -> NewState#state{successors=[Node]};
        _ -> NewState
    end,
	Reply = ok,
    {reply, Reply, NewState2};
    
% Get the immediate successor of this node
handle_call(successor, _From, State) ->
	Reply = call({successor, State#state.key}, State#state.predecessor),
    {reply, Reply, State};
    
%find_successor
handle_call({successor, Key}, _From, State) ->
    IsInBounds = (Key >= 0) and (Key =< ?MAX_KEY),
    Reply = case IsInBounds of
        true -> get_successor(Key, State);
        false -> {error, out_of_bounds}
    end,
    {reply, Reply, State};
    
% Change this node's successor
handle_call(stabilize, _From, State) ->
    NewState = stabilize(State),
	Reply = ok,
    {reply, Reply, NewState};

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

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the successor for the node when it starts up.
%% @spec init_successor(KnownNodes::[node()], key()) -> {ok, node()} |
%%                                                      {error, Reason}
%% @end
%%--------------------------------------------------------------------
init_successor([], _Key) ->
    [];

init_successor([KnownNode|Rest], Key) ->
    case call({successor, Key}, KnownNode) of
        {ok, Node} -> 
            io:format("Got successor for ~p: ~p~n", [Key, Node]),
            Node;
        {error, _Reason} -> init_successor(Rest, Key)
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc Gets the predecessor for the node's successor when it starts.
%% @spec init_predecessor(Successor::node()) -> node()
%% @end
%%--------------------------------------------------------------------
init_predecessor(Successor) ->
    case call(predecessor, Successor) of
        {ok, nil} -> Successor;
        {ok, Node} -> Node
    end.
    
    
    
get_successor(Key, State) ->
    case (State#state.predecessor =:= nil) or is_successor(State, Key) of
        true -> {ok, node()};
        false -> 
            % TODO: Optimize lookup by looking at finger table
            Predecessor = State#state.predecessor,
            call({successor, Key}, Predecessor)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Initialize the tables which store known nodes.
%% @spec init_finger_table(key()) -> [finger_record()]
%% @end
%%--------------------------------------------------------------------
init_finger_table(Key) when is_integer(Key) ->
	init_finger_table(Key, [], 0).
	
% create blank list of finger records
init_finger_table(Key, Successors, BitPos) when BitPos < ?HASH_LENGTH ->
	CurrentKey = next_finger(Key, BitPos),
	NextKey = next_finger(Key, BitPos + 1),
	NewSuccessor = {CurrentKey, {CurrentKey, NextKey}, node()},
	init_finger_table(Key, [NewSuccessor|Successors], BitPos + 1);
	
init_finger_table(_Key, Successors, ?HASH_LENGTH) ->
	lists:reverse(Successors).

%%--------------------------------------------------------------------
%% @private
%% @doc Determine if the node is the only node in the ring.
%%
%% @spec is_only_node(State) -> true | false
%% @end
%%--------------------------------------------------------------------
is_only_node(State) ->
    State#state.successors =:= [].
    
%%--------------------------------------------------------------------
%% @private
%% @doc Determine if the node is responsible for the key.
%%
%% @spec is_successor(State, Key::key()) -> true | false
%% @end
%%--------------------------------------------------------------------
is_successor(State, Key) ->
    Predecessor = State#state.predecessor,
    NodeKey = State#state.key,
    PredecessorKey = State#state.predecessor_key,
    IsFirstNode = PredecessorKey > NodeKey,
    HasNoPredecessor = Predecessor =:= nil,
    KeyIsNodeKey = Key =:= NodeKey,
    KeyIsInBetween = (Key > PredecessorKey) and (Key =< NodeKey),
    KeyIsBetweenFirstNode = (((Key > PredecessorKey) and (Key =< ?MAX_KEY)) or ((Key >= 0) and (Key =< NodeKey))) and IsFirstNode,
    IsSuccessor = HasNoPredecessor or       % Only Node
                  KeyIsNodeKey or           % Same key as node's key
                  KeyIsInBetween or         % Key between bredecessor and node's key
                  KeyIsBetweenFirstNode,    % Node is first (by key value) in the ring
    io:format("Check if ~p > ~p and ~p =< ~p: OR ~p < ~p -> ~p~n", [Key, PredecessorKey, Key, NodeKey, NodeKey, PredecessorKey, IsSuccessor]),
    IsSuccessor.
    
%%--------------------------------------------------------------------
%% @private
%% @doc Find the closest match to the key in the local finger table.
%%
%% @spec finger(Key, FingerTable::finger_list()) -> true | false
%% @end
%%--------------------------------------------------------------------
finger(Key, FingerTable=[]) when is_list(FingerTable) ->
    io:format("Key: ~p~n", [Key]),
    {error, key_out_of_bounds};

finger(Key, [{Start, {Start, End}, Node}|T]) ->
    io:format("Fingering node: ~p < ~p =< ~p...~n", [Start, Key, End]),
    case ((Start < Key) and (Key =< End)) or ((Start =< Key) and (Key > End)) of
        true ->
            Node;
        false ->
            finger(Key, T)
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc Calculates the BitPos-th key, based on the chord principle
%% that the finger table length should be m long where m is the 
%% number of bits in the hash algorithm used. If it exceeds the max
%% value for the hash type, it will cycle back to the beginning.
%% @spec next_finger(Key, BitPos) -> int()
%% @end
%%--------------------------------------------------------------------
next_finger(Key, BitPos) ->
    (Key + (1 bsl BitPos)) rem ?MAX_KEY.
    
%%--------------------------------------------------------------------
%% @private
%% @doc 
%% @spec stabilize(State) -> NewState
%% @end
%%--------------------------------------------------------------------
stabilize(State) ->
    Successor = hd(State#state.successors),
    {ok, SuccessorsPredecessor} = call(predecessor, Successor),
    case SuccessorsPredecessor of
        node -> State;
        NewSuccessor -> State#state{successors=[NewSuccessor]}
    end.

