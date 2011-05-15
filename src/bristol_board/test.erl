%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------

-module(test).

-export([init/0,
        test1/2]).
        
%% -include("./include/bb_include.hrl").

init() ->
    bb_database_event_logger:add_handler().

test1(Username, Password) ->        
    bb_database_server:is_user_valid(Username, Password).
    
    