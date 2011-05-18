%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------

-module(bb_database_event).

-export([start_link/0,
        add_handler/2,
        delete_handler/2,
        is_user_password_valid_call/2,
        is_user_password_valid_return/3,
        create_condition_report/3,
        get_condition_reports/3]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?MODULE, Handler, Args).

is_user_password_valid_call(Username, Password) ->
    gen_event:notify(?MODULE, {is_user_password_valid_call, {Username, Password}}).
is_user_password_valid_return(Username, Password, Return) ->
    gen_event:notify(?MODULE, {is_user_password_valid_return, {Username, Password, Return}}).    
    
create_condition_report(call, Username, Content) ->
    gen_event:notify(?MODULE, {create_condition_report, {call, Username, Content}}).
    
get_condition_reports(call, username, Username) ->
    gen_event:notify(?MODULE, {get_condition_reports, {call, username, Username}}).
