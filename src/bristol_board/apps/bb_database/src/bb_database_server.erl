%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% @doc Server that handles database calls to PostgreSQL and S3.
%% @end
%% ---------------------------------------------------------------------

-module(bb_database_server).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% Defines.
%% ------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(host, "localhost").
-define(port, 5432).
-define(username, "ubuntu").
-define(password, "password").
-define(database, "database").
-define(pool_size, 4).
-define(pgpool_name, postgresql_pool).

%% ------------------------------------------------------------------
%% Records.
%% ------------------------------------------------------------------
-record(state, {pgsql_pool}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_) ->
  {ok, P} = pgsql_pool:start_link(?pgpool_name,
                                  ?pool_size,
                                  [{host, ?host},
                                   {port, ?port},
                                   {username, ?username},
                                   {password, ?password},
                                   {database, ?database}]),
  {ok, C1} = get_connection(?pgpool_name),
  {ok, C2} = get_connection(?pgpool_name),
  {ok, C3} = get_connection(?pgpool_name),
  {ok, C4} = get_connection(?pgpool_name),
  
  %% AI interesting, fails in timeout rather than immediately when we
  %% exhaust the pool.  suggests we need to retry operations.
  %% {ok, C5} = get_connection(?pgpool_name),
  {ok, #state{pgsql_pool = P}}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  P = State#state.pgsql_pool,
  pgsql_pool:stop(P),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_connection(P) ->
    case pgsql_pool:get_connection(P, 1000) of
        {ok, C} ->            
            {ok, C};
        Error -> 
            Error
    end.
