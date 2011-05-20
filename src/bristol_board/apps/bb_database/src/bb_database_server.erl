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
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% User interface exports.  For use externally.
%% ------------------------------------------------------------------
-export([start_link/0,
        is_api_key_present/1,
        is_api_key_valid/1,        
        is_user_password_valid/2,
        sleep/0,
        create_condition_report/2,
        get_condition_reports/2]).

%% ------------------------------------------------------------------
%% Exports only intended for internal use.  Required to permit
%% spawn_link()-ing.
%% ------------------------------------------------------------------
-export([query_sleep/2,
        query_is_user_password_valid/4,
        query_create_condition_report/4,
        query_get_condition_reports/4]).

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
create_condition_report(Username, Contents) ->
    gen_server:call(?MODULE, {create_condition_report, Username, Contents}, ?DB_TIMEOUT).
get_condition_reports(username, Username) ->
    gen_server:call(?MODULE, {get_condition_reports, username, Username}, ?DB_TIMEOUT).    
    
	  
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @doc Initiqlisation.  We trap exits and explicitly handle them in
%% handle_info, so that we can no die on query timeouts.
init(_Args) ->
    process_flag(trap_exit, true),    
    
    %% The last 0 is a timeout that triggers a handle_info timeout call.
    {ok, #state{}, 0}.

handle_call({sleep}, From, State) ->     
    proc_lib:spawn_link(?MODULE, query_sleep, [From, ?pgpool_auth_name]),    
    {noreply, State};
    
handle_call({is_user_password_valid, Username, Password}, From, State) -> 
    bb_database_event:is_user_password_valid_call(Username, Password),   
    proc_lib:spawn_link(?MODULE, query_is_user_password_valid, [From, ?pgpool_auth_name, Username, Password]),    
    {noreply, State};
    
handle_call({create_condition_report, Username, Contents}, From, State) -> 
    bb_database_event:create_condition_report(call, Username, Contents),   
    proc_lib:spawn_link(?MODULE, query_create_condition_report, [From, ?pgpool_ops_name, Username, Contents]),    
    {noreply, State};    
    
handle_call({get_condition_reports, username, Username}, From, State) -> 
    bb_database_event:get_condition_reports(call, username, Username),   
    proc_lib:spawn_link(?MODULE, query_get_condition_reports, [From, ?pgpool_ops_name, username, Username]),    
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
    io:format("Query stops. Pid: ~p, Reason: ~p~n", [Pid, Reason]),
    {noreply, State};

%% @doc Receive callbacks from proc_lib:init_ack() calls from
%% spawned query processes.
%%
%% proc_lib:init_ack() returns {ack, Pid(), <return value>}
%% where <return valud> is Returncode
handle_info({ack, QueryPid, {query_pid, ok}}, State) ->
    io:format("ok ack. QueryPid: ~p~n", [QueryPid]),
    {noreply, State}; 
handle_info({ack, QueryPid, ReturnCode}, State) ->
    io:format("bad ack. QueryPid: ~p, ReturnCode: ~p~n", [QueryPid, ReturnCode]),
    {noreply, State};        

%% @doc Result of the 0 timeout init call.  Set up the event logger.
handle_info(timeout, State) ->
    bb_database_event_logger:add_handler(),
    {noreply, State};
    
handle_info(Info, State) ->
    io:format("Unknown info. Info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Queries.  These are any kind of database executions, regardless
%% of whether they answer questions, alter data, etc.
%% ------------------------------------------------------------------    

%% @doc Cheeky function.  Just sleeps for 10 seconds, hence overruns
%% the default gen_server timeout of 5000ms and triggers a
%% timeout _on the caller_.  Interesting is that the process running
%% this function will exit with a normal reason.
query_sleep(_From, Pool) ->
    proc_lib:init_ack(ok),
    _Result = q(Pool, "SELECT pg_sleep(10);", []).    

%% @doc Is the username and password combination valid?  Note the
%% catch that we're using a PostgreSQL contrib module to provide
%% native access to bcrypt password hashing.
%%
%% Reference: http://www.postgresql.org/docs/9.0/interactive/pgcrypto.html
query_is_user_password_valid(From, Pool, Username, Password) ->    
    proc_lib:init_ack({query_pid, ok}),
    Result = q(Pool, "SELECT true FROM articheck_user WHERE username = $1 AND password = crypt($2, password)", [Username, Password]),        
    case Result of
        {ok, [<<"bool">>], [{true}]} ->
            bb_database_event:is_user_password_valid_return(Username, Password, {ok, true}),   
            gen_server:reply(From, {return_query, ok, true});
        {ok, [<<"bool">>], [{false}]} ->
            bb_database_event:is_user_password_valid_return(Username, Password, {ok, false}),   
            gen_server:reply(From, {return_query, ok, false});
        {error, Reason} ->
            bb_database_event:is_user_password_valid_return(Username, Password, {error, Reason}),   
            gen_server:reply(From, {return_query, error, Reason})
    end.        
    
query_create_condition_report(From, Pool, Username, Contents) ->    
    proc_lib:init_ack({query_pid, ok}),
    RUserId = q(Pool, "SELECT user_id FROM articheck_user WHERE username = $1", [Username]),        
    case RUserId of
        {ok, [{UserId}]} ->
            RConditionReport = q(Pool, "INSERT INTO condition_report "
                                    "(revision_id, condition_report_id, user_id, "
                                    "datetime_edited, contents) VALUES "
                                    "(uuid_generate_v4(), uuid_generate_v4(), $1, "
                                    "now(), $2)", [UserId, Contents]),
            case RConditionReport of
                {error, Reason} ->
                    gen_server:reply(From, {return_query, error, Reason});
                _ ->
                    gen_server:reply(From, {return_query, ok})
            end;
        {error, Reason} ->
            gen_server:reply(From, {return_query, error, Reason})
    end.            
    
query_get_condition_reports(From, Pool, username, Username) ->    
    proc_lib:init_ack({query_pid, ok}),    
    RQuery = q(Pool, "SELECT C.revision_id, C.condition_report_id, C.contents, C.datetime_edited
                       FROM condition_report C
                       INNER JOIN (
                           SELECT condition_report_id, MAX(datetime_edited) AS datetime_edited
                           FROM condition_report
                           WHERE user_id = (SELECT user_id FROM articheck_user WHERE username = $1)
                           GROUP BY condition_report_id
                       ) X
                       ON X.condition_report_id = C.condition_report_id AND
                          X.datetime_edited = C.datetime_edited", [Username]),            
    case RQuery of
        {ok, Cols, Rows} ->
            %% Format the last element of each row, i.e. the datetime.
            RowsDate = [ [element(1, Row),
                         element(2, Row),
                         element(3, Row),
                         erlang:iolist_to_binary(dh_date:format("Y-m-d H:i:s", element(4, Row)))] || Row <- Rows ],
            io:format("RowsDate: ~n~p~n", [RowsDate]),
        
            %% For each row zip it with the Columns, hence
            %% replicating the column name in each row.
            Rows2 = [ lists:zip(Cols, Row) || Row <- RowsDate ],        
            gen_server:reply(From, {return_query, ok, Rows2});
        {error, Reason} ->
            gen_server:reply(From, {return_query, error, Reason})
    end.                
    
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------        
q(P, Sql, Parameters) ->
    {ok, C} = get_connection(P),
    try
        case pgsql:equery(C, Sql, Parameters) of
            {ok, Cols, Rows} ->
                %% Get the column name from Cols.
                %%
                %% List comprehension over the list of tuples.
                %% Get the second element of each tuple in this
                %% list of tuples.
                Cols2 = [ element(2, Elem) || Elem <- Cols ],
                {ok, Cols2, Rows};
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
