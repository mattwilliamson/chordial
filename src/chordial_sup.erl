%%%----------------------------------------------------------------
%%% @author  Matt Williamson <dawsdesign@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2009 Matt Williamson
%%%----------------------------------------------------------------
-module(chordial_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    error_logger:info_msg("Starting supervisor...~n"),
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    KnownNodes = case application:get_env(chordial, nodes) of
        {ok, Nodes} -> Nodes;
        undefined -> []
    end,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    ChordialServer = {gen_chord, {gen_chord, start_link, [KnownNodes]},
              Restart, Shutdown, Type, [gen_chord]},
    MonitorServer = {chord_monitor, {chord_monitor, start_link, []},
              Restart, Shutdown, Type, [chord_monitor]},
    error_logger:info_msg("Started supervisor.~n"),
    {ok, {SupFlags, [MonitorServer, ChordialServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


