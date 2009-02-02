%%%-------------------------------------------------------------------
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%% @copyright (C) 2009, Matt Williamson
%%% @doc This module perfoms common tasks for chord.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chord_lib).

-include("types.hrl").

-define('HASH_LENGTH', 160). % sha1
-define('MAX_KEY', max_hash_value(?HASH_LENGTH)).

%% API
-export([init_finger/1, init_successors/1, hash/1, find_successor/2,
        find_predecessor/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Initialize the tables which store known nodes.
%% @spec init_finger(key()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
init_finger(Key) when is_integer(Key) ->
	init_finger(Key, [], 0).
	
% create blank list of finger records
init_finger(Key, Successors, BitPos) when BitPos < ?HASH_LENGTH ->
	CurrentKey = next_finger(Key, BitPos),
	NextKey = next_finger(Key, BitPos + 1),
	NewSuccessor = {CurrentKey, {CurrentKey, NextKey}, node()},
	init_finger(Key, [NewSuccessor|Successors], BitPos + 1);
	
init_finger(_Key, Successors, ?HASH_LENGTH) ->
	lists:reverse(Successors).
	
%%--------------------------------------------------------------------
%% @doc Initialize the tables which store known nodes.
%% @spec init_successors(key()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
init_successors(KnownNodes) when is_list(KnownNodes) ->
	[].
	
%%--------------------------------------------------------------------
%% @doc Hashes node address or content
%% @spec hash(String) -> {ok, Hash} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
hash(String) when is_list(String) ->
	<<HashInteger:?HASH_LENGTH>> = sha1:binstring(String),
	HashInteger.
	
%%--------------------------------------------------------------------
%% @doc Finds the successor for a given key.
%% @spec find_successor(Key::integer(), finger_list()) -> {key(), node()}
%% @end
%%--------------------------------------------------------------------
find_successor(_Key, []) ->
    {error, key_out_of_bounds};

find_successor(Key, [{Start, {Start, End}, Node}|T]) ->
    case (Start < Key) and (Key =< End) of
        true ->
            {Key, Node};
        false ->
            find_successor(Key, T)
    end.
    
%%--------------------------------------------------------------------
%% @doc Finds the predecessor for a given key.
%% @spec find_predecessor(Key::integer(), finger_list()) -> {key(), node()}
%% @end
%%--------------------------------------------------------------------
find_predecessor(_Key, []) ->
    {error, key_out_of_bounds};

find_predecessor(Key, [{_Start, {_Start, _End},  Node}, 
                       N={ Start, { Start,  End}, _Node}
                       |T]) ->
    case (Start < Key) and (Key =< End) of
        true ->
            {Key, Node};
        false ->
            find_predecessor(Key, [N|T])
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
	
%%--------------------------------------------------------------------
%% @private
%% @doc Calculates the maximum hash value given the bit length of the
%% hash.
%% @spec max_hash_value(BitCount) -> integer()
%%      where BitCount = integer()
%% @end
%%--------------------------------------------------------------------
max_hash_value(BitCount) ->
    max_hash_value(BitCount, 0, 0).
    
max_hash_value(BitCount, Total, Pos) when Pos < BitCount ->
    NewTotal = Total + (1 bsl Pos),
    NewPos = Pos + 1,
    max_hash_value(BitCount, NewTotal, NewPos);
    
max_hash_value(_, Total, _) ->
    Total.
    
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

