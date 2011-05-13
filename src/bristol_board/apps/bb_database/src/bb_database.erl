%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------

%% @doc bb_database startup code

-module(bb_database).
-author('Asim Ihsan <asim.ihsan@gmail.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    bb_database_sup:start_link().

%% @spec start() -> ok
%% @doc Start the bb_database server.
start() ->
    ensure_started(crypto),
    application:start(bb_database).

%% @spec stop() -> ok
%% @doc Stop the bb_database server.
stop() ->
    Res = application:stop(bb_database),
    application:stop(crypto),
    Res.

