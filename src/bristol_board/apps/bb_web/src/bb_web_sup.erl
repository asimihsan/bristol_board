%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------

%% @doc Supervisor for the bb_web application.

-module(bb_web_sup).
-author('Asim Ihsan <asim.ihsan@gmail.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("WEB_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "dispatch.conf"])),
    Port = case os:getenv("WEB_PORT") of false -> "8080"; Any2 -> Any2 end,
    WebConfig = [
        {name, one},
        {ip, Ip},
        {port, Port},
        {log_dir, "priv/log"},
        {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
    
    Port2 = case os:getenv("WEB_SSL_PORT") of false -> "8443"; Any3 -> Any3 end,    
    WebSSLConfig = [
        {name, two},
        {ip, Ip},
        {port, Port2},
        {ssl, true},
        {ssl_opts, [{certfile,   "/home/ubuntu/myCA/server_crt.crt"},
                   {cacertfile, "/home/ubuntu/myCA/cacert.pem"},
                   {keyfile,    "/home/ubuntu/myCA/server_key.pem"}]},
        {log_dir, "priv/log"},
        {dispatch, Dispatch}],
    WebSSL = {two,
          {webmachine_mochiweb, start, [WebSSLConfig]},
          permanent, 5000, worker, dynamic},
    Processes = [Web, WebSSL],           

    {ok, { {one_for_one, 10, 10}, Processes} }.
