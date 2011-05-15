%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------

-module(bb_database_sup).

-behaviour(supervisor).

%% ------------------------------------------------------------------
%% Includes.
%% ------------------------------------------------------------------
-include("../include/bb_database_include.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Server = ?CHILD(bb_database_server, worker),
    
    DbOpts = [{host, ?host},
              {port, ?port},
              {username, ?username},
              {password, ?password},
              {database, ?database}],
    PgPoolAuth = {?pgpool_auth_name,
                 {pgsql_pool, start_link, [?pgpool_auth_name,
                                        ?pgpool_auth_size,
                                        DbOpts]},
                permanent, 5000, worker, dynamic},    
    PgPoolOps = {?pgpool_ops_name,
                 {pgsql_pool, start_link, [?pgpool_ops_name,
                                        ?pgpool_ops_size,
                                        DbOpts]},
                permanent, 5000, worker, dynamic},                
    EventManager = ?CHILD(bb_database_event, worker),
                
    Processes = [Server, PgPoolAuth, PgPoolOps, EventManager],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Processes} }.
