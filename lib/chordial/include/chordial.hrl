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
-define('HASH_LENGTH', 160). % sha1
