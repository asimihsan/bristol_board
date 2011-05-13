%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------

%% @doc Webmachine resource that handles snippet-related actions

-module(bb_web_snippet_resource).
-author('Asim Ihsan <asim.ihsan@gmail.com>').

-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
  %% PathInfo = wrq:path_info(ReqData),
  %% {ok, SnippetKey} = dict:find(key, PathInfo),
  %% {snippet, SnippetData} = bb_core_server:get_snippet(list_to_binary(SnippetKey)),
  %% {ok, Content} = snippet_dtl:render(SnippetData),
  Content = <<"this is a snippet">>,
  {Content, ReqData, State}.

