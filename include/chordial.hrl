%%%-------------------------------------------------------------------
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%% @copyright (C) 2009, Matt Williamson
%%% @doc This include file contains common constants, and
%%% documentation types.
%%%
%%% @end
%%%-------------------------------------------------------------------

%%%===================================================================
%%% Documentation Types
%%%===================================================================

%% @type node() = atom().
%% @type key() = integer().
%% @type finger_list() = [finger_record()].
%% @type finger_record() = {StartKey::integer(), 
%%                          {StartKey::integer(), EndKey::integer()}, 
%%                          Node::atom()}.


%%%===================================================================
%%% Constants
%%%===================================================================
-define(debug, true).
-define(max_successors, 8).
-define(stabilize_delay, 10000).
-define(fix_keys_delay, 10000).

-ifdef(debug).
    -define(hash_len, 8).
-else.
    -define(hash_len, 160). % sha1
-endif.