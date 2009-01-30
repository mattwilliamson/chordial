%%%-------------------------------------------------------------------
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%% @copyright (C) 2009, Matt Williamson
%%% @doc This module provides a behaviour for nodes implementing
%%% the chord DHT algorithm. It provides an interface for callbacks
%%% such as nodes joining or leaving the DHT. This allows the client
%%% of chord to do things like start replication when a node joins
%%% or start a failover process when one leaves.
%%%
%%% @end
%%% Created : 30 Jan 2009 by Matt Williamson <mwilliamson@dawsdesign.com>
%%%-------------------------------------------------------------------
-module(gen_chord).

%% API
-export([behaviour_info/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Get list of callbacks for the chord behaviour.
%% @spec behaviour_info(callbacks) -> [{CallbackFun, Arity}]
%% where
%%   CallbackFun = atom(),
%%   Arity = integer()
%% @end
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{new_predecessor, 1}, {new_successor, 1}, {get, 1}, {put, 2}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
