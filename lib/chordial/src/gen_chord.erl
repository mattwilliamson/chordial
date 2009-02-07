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

-record(state, {key, predecessor=nil, finger_table=[], successors=[]}).

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
	FingerTable = init_finger_table(NodeKey),
	Successors = case KnownNodes of
	    [] -> [];
	    _ -> [init_successor(KnownNodes, NodeKey)]
	end,
	Predecessor = case Successors of
	    [] -> nil;
	    _ -> init_predecessor(hd(Successors))
	end,
	case Successors /= [] of
	    true -> call({notify, node()}, hd(Successors));
	    _ -> ok
	end,
    {ok, #state{key=NodeKey, 
                finger_table=FingerTable,
                successors=Successors,
                predecessor=Predecessor}}.

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
	Reply = {ok, finger(State#state.finger_table, Key)},
    {reply, Reply, State};
    
% Get the immediate predecessor of this node
handle_call(predecessor, _From, State) ->
	Reply = {ok, State#state.predecessor},
    {reply, Reply, State};
    
% Change this node's predecessor
handle_call({notify, Node}, _From, State) ->
    NewState = State#state{predecessor=Node},
    NewState2 = case State#state.successors of
        [] -> NewState#state{successors=[Node]};
        _ -> State
    end,
	Reply = ok,
    {reply, Reply, NewState2};
    
% Get the immediate successor of this node
handle_call(successor, _From, State) ->
	Reply = {ok, hd(State#state.successors)},
    {reply, Reply, State};
    
%find_successor
handle_call(Request={successor, Key}, _From, State) ->
    Reply = case is_only_node(State) or is_successor(State, Key) of
        true -> {ok, node()};
        false -> 
            ClosestFinger = finger(State#state.finger_table, Key),
            call(Request, ClosestFinger)
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
        {ok, Node} -> Node;
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
    N = State#state.key,
    (Key > Predecessor) and (Key =< N).
    
%%--------------------------------------------------------------------
%% @private
%% @doc Find the closest match to the key in the local finger table.
%%
%% @spec finger(FingerTable::finger_list(), Key) -> true | false
%% @end
%%--------------------------------------------------------------------
finger(FingerTable=[], _Key) when is_list(FingerTable) ->
    {error, key_out_of_bounds};

finger([{Start, {Start, End}, Node}|T], Key) ->
    case (Start < Key) and (Key =< End) of
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
    Key + ((1 bsl BitPos) rem ?MAX_KEY).
    
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

