%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------

-module(bb_database_event).

-export([start_link/0,
        add_handler/2,
        delete_handler/2,
        is_user_valid_call/2,
        is_user_valid_return/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

is_user_valid_call(Username, Password) ->
    gen_event:notify(?SERVER, {is_user_valid_call, {Username, Password}}).
is_user_valid_return(Username, Password, Return) ->
    gen_event:notify(?SERVER, {is_user_valid_return, {Username, Password, Return}}).    
