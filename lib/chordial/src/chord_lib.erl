%%%-------------------------------------------------------------------
%%% @author Matt Williamson <dawsdesign@gmail.com>
%%% @copyright (C) 2009, Matt Williamson
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chord_lib).

-define('HASH_LENGTH', 160). % sha1
-define('MAX_KEY', max_hash_value(?HASH_LENGTH)).

%% API
-export([init_tables/1, hash/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Initialize the tables which store known nodes.
%% @spec init_tables(Key) -> ok | {error, Reason}
%% where Key = integer()
%% @end
%%--------------------------------------------------------------------
init_tables(Key) when is_integer(Key) ->
	ets:new(finger, [named_table]),
	init_tables(Key, [], 0).
	
%%--------------------------------------------------------------------
%% @doc Hashes node address or content
%% @spec hash(String) -> {ok, Hash} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
hash(String) when is_list(String) ->
	<<HashInteger:?HASH_LENGTH>> = sha1:binstring(String),
	HashInteger.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% create list of finger records
init_tables(Key, Records, Acc) when Acc < ?HASH_LENGTH ->
	Offset = (1 bsl Acc), % 1, 2, 4, 8, 16x10 (1, 10, 100, 1000, 10000x2)...
	CurrentKey = case Key + Offset < ?MAX_KEY of
		true -> 
		    Key + Offset;
		false -> 
		    Key + Offset - ?MAX_KEY
	end,
	NewRecord = {CurrentKey, CurrentKey},
	init_tables(Key, [NewRecord|Records], Acc + 1);
	
% insert list of finger records into table
init_tables(_Key, Records, ?HASH_LENGTH) ->
	ets:insert(finger, Records).
	
max_hash_value(BitCount) ->
    max_hash_value(BitCount, 0, 0).
    
max_hash_value(BitCount, Total, Pos) when Pos < BitCount ->
    NewTotal = Total + (1 bsl Pos),
    NewPos = Pos + 1,
    max_hash_value(BitCount, NewTotal, NewPos);
    
max_hash_value(_, Total, _) ->
    Total.
    
    

