%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------

-module(test).

-export([init/0,
        test1/2,
        test2/0]).
        
%% -include("./include/bb_include.hrl").

init() ->
    bb_database_event_logger:add_handler().

test1(Username, Password) ->            
    bb_database_server:is_user_password_valid(Username, Password).
    
test2() ->            
    bb_database_server:sleep().    
    
    