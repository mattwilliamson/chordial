%%%-------------------------------------------------------------------
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%% @copyright (C) 2009, Matt Williamson
%%% @doc This module perfoms common tasks for chord.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chord_lib).

-include("chordial.hrl").

%% API
-export([hash/1, max_hash_value/0, max_hash_value/1]).

%%%===================================================================
%%% API
%%%===================================================================
	
%%--------------------------------------------------------------------
%% @doc Hashes node address or content
%% @spec hash(String) -> {ok, Hash} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
hash(Node) when is_atom(Node) ->
    hash(atom_to_list(Node));
    
hash(String) when is_list(String) ->
	Hash = sha1:binstring(String),
	GarbageBits = 160 - ?hash_len,
	case GarbageBits =:= 0 of
	    true -> <<TruncatedHash:?hash_len>> = Hash;
	    false -> <<_:GarbageBits, TruncatedHash/unsigned-integer>> = Hash
	end,
	TruncatedHash.
	
%%--------------------------------------------------------------------
%% @doc Calculates the maximum hash value
%% hash.
%% @spec max_hash_value(BitCount) -> integer()
%%      where BitCount = integer()
%% @end
%%--------------------------------------------------------------------
max_hash_value() ->
    max_hash_value(?hash_len).

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

