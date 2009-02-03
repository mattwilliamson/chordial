%%%-------------------------------------------------------------------
%%% @author Matt Williamson <dawsdesign@gmail.com>
%%% @copyright (C) 2009, Matt Williamson
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chord_lib).

-define('HASH_LENGTH', 160). % sha1
-define('MAX_KEY', math:pow(2, ?HASH_LENGTH)).

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
	ets:new(finger, []),
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
	Offset = Key + (1 bsl Acc), % 1, 2, 4, 8, 16x10 (1, 10, 100, 1000, 10000x2)...
	CurrentKey = case (Key + Offset) < ?MAX_KEY of
		true -> Key + Offset;
		false -> Key + Offset - ?MAX_KEY
	end,
	init_tables(Key, [{CurrentKey, testvalue}|Records], Acc + 1);
	
% insert list of finger records into table
init_tables(_Key, Records, ?HASH_LENGTH) ->
	ets:insert(finger, Records).

