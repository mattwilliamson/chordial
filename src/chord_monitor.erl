%%%-------------------------------------------------------------------
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%% @copyright (C) 2009, Matt Williamson
%%% @doc This module monitors the chordial_sup named process on nodes
%%% the calling supervisor is interested in.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chord_monitor).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% API
-export([start_link/0, watch/1]).

-define('PROCESS_TO_MONITOR', chordial_sup).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Gen server callbacks
init([]) ->
    error_logger:info_msg("Starting monitor...~n"),
    WorkerPid = spawn_link(fun() -> monitor_loop([]) end),
    error_logger:info_msg("Started monitor.~n"),
    {ok, WorkerPid}.
    
% Add a node to monitor the chordial_sup process on
handle_call(Request={monitor, _Node}, _From, WorkerPid) ->
    WorkerPid ! Request,
    Reply = ok,
    {reply, Reply, WorkerPid};
    
% A node or chordial_sup was reported down
handle_call(Request={nodedown, _Node}, _From, WorkerPid) ->
    gen_chord:call(Request),
    Reply = ok,
    {reply, Reply, WorkerPid};
    
% Unkown Call
handle_call(_Request, _From, WorkerPid) ->
    Reply = {error, unknown_call},
    {reply, Reply, WorkerPid}.
    
handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%API
monitor_loop(Nodes) ->
    receive 
        {monitor, Node} ->
            error_logger:info_msg("Monitor: Begin monitoring ~p...~n", [Node]),
            erlang:monitor(process, {?PROCESS_TO_MONITOR, Node}),
            error_logger:info_msg("Monitor: Began monitoring ~p.~n", [Node]),
            monitor_loop([Node|Nodes]);
        {'DOWN', _MonitorRef, process, {?PROCESS_TO_MONITOR, Node}, _Info} ->
            error_logger:info_msg("Monitor: Node down (~p)~n", [Node]),
            gen_server:call(?MODULE, {nodedown, Node})
    end,
    monitor_loop(Nodes).
    
watch(Node) ->
    gen_server:call(?MODULE, {monitor, Node}).