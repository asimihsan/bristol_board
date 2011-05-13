%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------

-module(bb_core_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_snippet/1, save_snippet/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  %% ConnInfo = bb_riak_config:connection_info(),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

save_snippet(Snippet) ->
  gen_server:call(?SERVER, {save_snippet, Snippet}, infinity).

get_snippet(SnippetKey) ->
  gen_server:call(?SERVER, {get_snippet, SnippetKey}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, []}.

%% handle_call({save_snippet, Snippet}, _From, ConnInfo) ->
%%   RiakPid = bb_riak:connect(ConnInfo),
%%   SavedSnippet = bb_snippet:save(RiakPid, Snippet),
%%   {reply, SavedSnippet, ConnInfo};

%% handle_call({get_snippet, SnippetKey}, _From, ConnInfo) ->
%%  RiakPid = bb_riak:connect(ConnInfo),
%%  Snippet = bb_snippet:fetch(RiakPid, SnippetKey),
%%  {reply, Snippet, ConnInfo};

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

