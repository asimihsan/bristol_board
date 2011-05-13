%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------
%% @doc Callbacks for the bb_web application.

-module(bb_web_app).
-author('Asim Ihsan <asim.ihsan@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for bb_web.
start(_Type, _StartArgs) ->
    bb_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for bb_web.
stop(_State) ->
    ok.
