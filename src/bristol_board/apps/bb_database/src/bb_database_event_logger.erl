%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------

-module(bb_database_event_logger).

-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2,
        handle_info/2, code_change/3, terminate/2]).

-record(state, {}).

add_handler() ->
    bb_database_event:add_handler(?MODULE, []).

delete_handler() ->
    bb_database_event:delete_handler(?MODULE, []).

init([]) ->
    {ok, #state{}}.

handle_event({is_user_valid_call, {Username, Password}}, State) ->
    error_logger:info_msg("bb_database::is_user_valid(~p, ~p) call~n", [Username, Password]),
    {ok, State};
handle_event({is_user_valid_return, {Username, Password, Return}}, State) ->
    error_logger:info_msg("bb_database::is_user_valid(~p, ~p) return: ~p~n", [Username, Password, Return]),
    {ok, State}.    

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
