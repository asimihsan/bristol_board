%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% @doc Server that handles database calls to PostgreSQL and S3.
%% @end
%% ---------------------------------------------------------------------

-module(bb_database_server).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% Includes.
%% ------------------------------------------------------------------
-include("../include/bb_database_include.hrl").

%% ------------------------------------------------------------------
%% Records.
%% ------------------------------------------------------------------
-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0,
        is_api_key_present/1,
        is_api_key_valid/1,        
        is_user_password_valid/2,
        
        query_is_user_password_valid/4]).

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

is_api_key_present(ApiKey) ->
    gen_server:call(?MODULE, {is_api_key_present, ApiKey}).
is_api_key_valid(ApiKey) ->
    gen_server:call(?MODULE, {is_api_key_valid, ApiKey}).
is_user_password_valid(Username, Password) ->
    gen_server:call(?MODULE, {is_user_password_valid, Username, Password}, ?DB_TIMEOUT).
	  
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_Args) ->
    {ok, #state{}}.

handle_call({is_user_valid, Username, Password}, From, State) ->    
    bb_database_event:is_user_password_valid(Username, Password),   
    proc_lib:spawn(?MODULE, query_is_user_password_valid, [From, ?pgpool_auth_name, Username, Password]),
    {noreply, State};
    
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Queries.
%% ------------------------------------------------------------------    
query_is_user_password_valid(From, Pool, Username, Password) ->
    Result = q(Pool, "SELECT true FROM articheck_user WHERE username = $1 AND password = crypt($2, password)", [Username, Password]),        
    case Result of
        [{true}] ->
            bb_database_event:is_user_password_valid_return(Username, Password, {ok, true}),   
            gen_server:reply(From, {ok, true});
        _ ->
            bb_database_event:is_user_password_valid_return(Username, Password, {ok, false}),   
            gen_server:reply(From, {ok, false})
    end.        
    
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------        
q(P, Sql, Parameters) ->
    {ok, C} = get_connection(P),
    try
        case pgsql:equery(C, Sql, Parameters) of
            {ok, _Cols, Rows} -> Rows;
            {ok, Rows} -> Rows
        end
    after
        return_connection(P, C)
    end.
    
get_connection(P) ->
    pgsql_pool:get_connection(P, 1000).

return_connection(P, C) ->
    pgsql_pool:return_connection(P, C).
