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
        sleep/0,
        
        query_sleep/2,
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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

is_api_key_present(ApiKey) ->
    gen_server:call(?MODULE, {is_api_key_present, ApiKey}).
is_api_key_valid(ApiKey) ->
    gen_server:call(?MODULE, {is_api_key_valid, ApiKey}).
is_user_password_valid(Username, Password) ->    
    gen_server:call(?MODULE, {is_user_password_valid, Username, Password}, ?DB_TIMEOUT).
sleep() ->    
    gen_server:call(?MODULE, {sleep}, ?DB_TIMEOUT).    
	  
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @doc Initiqlisation.  We trap exits and explicitly handle them in
%% handle_info, so that we can no die on query timeouts.
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({sleep}, From, State) ->     
    proc_lib:spawn_link(?MODULE, query_sleep, [From, ?pgpool_auth_name]),    
    {noreply, State};
    
handle_call({is_user_password_valid, Username, Password}, From, State) -> 
    bb_database_event:is_user_password_valid_call(Username, Password),   
    proc_lib:spawn_link(?MODULE, query_is_user_password_valid, [From, ?pgpool_auth_name, Username, Password]),    
    {noreply, State};
    
handle_call(Request, From, State) ->
    io:format("Unknown call. Request: ~p, From: ~p~n", [Request, From]),
    {noreply, State}.

handle_cast(Msg, State) ->
    io:format("Unknown cast. Msg: ~p,~n", [Msg]),
    {noreply, State}.

%% @doc Handle exits from the processes we spawn_link to handle the
%% database queries for us.
handle_info({'EXIT', Pid, Reason}, State) ->
    io:format("Query timeout. Pid: ~p, Reason: ~p~n", [Pid, Reason]),
    {noreply, State};

%% @doc Receive callbacks from proc_lib:init_ack() calls from
%% spawned query processes.
%%
%% proc_lib:init_ack() returns {ack, Pid(), <return value>}
%% where <return valud> is Returncode
handle_info({ack, QueryPid, ok}, State) ->
    io:format("ok ack. QueryPid: ~p~n", [QueryPid]),
    {noreply, State}; 
handle_info({ack, QueryPid, ReturnCode}, State) ->
    io:format("bad ack. QueryPid: ~p, ReturnCode: ~p~n", [QueryPid, ReturnCode]),
    {noreply, State};        
    
handle_info(Info, State) ->
    io:format("Unknown info. Info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Queries.
%% ------------------------------------------------------------------    
query_sleep(_From, Pool) ->
    proc_lib:init_ack(ok),
    _Result = q(Pool, "SELECT pg_sleep(10);", []).    

query_is_user_password_valid(From, Pool, Username, Password) ->    
    proc_lib:init_ack({query_pid, ok}),
    Result = q(Pool, "SELECT true FROM articheck_user WHERE username = $1 AND password = crypt($2, password)", [Username, Password]),        
    case Result of
        {ok, [{true}]} ->
            bb_database_event:is_user_password_valid_return(Username, Password, {ok, true}),   
            gen_server:reply(From, {ok, true});
        {ok, _} ->
            bb_database_event:is_user_password_valid_return(Username, Password, {ok, false}),   
            gen_server:reply(From, {ok, false});
        {error, Reason} ->
            gen_server:reply(From, {error, Reason})
    end.        
    
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------        
q(P, Sql, Parameters) ->
    {ok, C} = get_connection(P),
    try
        case pgsql:equery(C, Sql, Parameters) of
            {ok, _Cols, Rows} -> {ok, Rows};
            {ok, Rows} -> {ok, Rows};
            {error, Reason} -> {error, Reason}
        end
    after
        return_connection(P, C)
    end.
    
get_connection(P) ->
    pgsql_pool:get_connection(P, 1000).

return_connection(P, C) ->
    pgsql_pool:return_connection(P, C).
