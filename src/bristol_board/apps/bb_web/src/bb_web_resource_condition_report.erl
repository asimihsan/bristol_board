%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------

%% @doc Webmachine resource that handles document creation.

%% References:

%% git://github.com/basho/riak.git › apps › riak_kv › src › riak_kv_wm_stats.erl

-module(bb_web_resource_condition_report).
-export([init/1,
        encodings_provided/2,
        content_types_provided/2,
        produce_body/2,
        allowed_methods/2,
        process_post/2,
        is_authorized/2]).
-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
    {{trace, "/tmp"}, Config}. %% debugging code.
    %%{ok, no_state}.          %% regular code.

%% @spec encodings_provided(webmachine:wrq(), context()) ->
%%         {[encoding()], webmachine:wrq(), context()}
%% @doc Get the list of encodings this resource provides.
%%      "identity" is provided for all methods, and "gzip" is
%%      provided for GET as well
encodings_provided(ReqData, Context) ->
    case wrq:method(ReqData) of
        'GET' ->
            {[{"identity", fun(X) -> X end},
              {"gzip", fun(X) -> zlib:gzip(X) end}], ReqData, Context};
        _ ->
            {[{"identity", fun(X) -> X end}], ReqData, Context}
    end.
    
%% @spec content_types_provided(webmachine:wrq(), context()) ->
%%          {[ctype()], webmachine:wrq(), context()}
%% @doc Get the list of content types this resource provides.
%%      "application/json" and "text/plain" are both provided
%%      for all requests.  "text/plain" is a "pretty-printed"
%%      version of the "application/json" content.
content_types_provided(ReqData, Context) ->
    {[{"application/json", produce_body},
      {"text/plain", produce_body}],
     ReqData, Context}.
    
allowed_methods(ReqData, Context) ->
    {['GET','POST'], ReqData, Context}.

is_authorized(ReqData, Context) ->
    common_resource_functions:is_authorized(ReqData, Context).
    
process_post(ReqData, Context) ->
    Params = common_resource_functions:get_json_dict(ReqData),
    Username = dict:fetch(username, Params),
    DocumentBlob = dict:fetch(document_blob, Params),
    JsonDocumentBlob = bb_json:to_json(DocumentBlob),
    {return_query, ok} = bb_database_server:create_condition_report(Username, JsonDocumentBlob),    
    io:format("JsonDocumentBlob: ~p~n", [JsonDocumentBlob]),        
    
    %%NewRd = wrq:append_to_response_body(io_lib:format("<html>testing append.  ReqData: ~p</html>", [ReqData]), ReqData),
    
    NewReqData = wrq:append_to_response_body(io_lib:format("<html>testing append.~n~n", []), ReqData),
    {true, NewReqData, Context}.

produce_body(ReqData, Context) ->
    Params = common_resource_functions:get_json_dict(ReqData),
    case dict:fetch(get_using, Params) of
        "username" ->
            Username = dict:fetch(username, Params),
            {return_query, ok, ConditionReports} = bb_database_server:get_condition_reports(username, Username),
            io:format("ConditionReports: ~p~n", [ConditionReports])      
    end,
    Result = [{condition_reports, bb_json:to_json(ConditionReports)}],
    {Result, ReqData, Context}.
