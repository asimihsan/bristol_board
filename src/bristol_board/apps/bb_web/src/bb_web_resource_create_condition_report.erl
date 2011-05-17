%% ---------------------------------------------------------------------
%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% ---------------------------------------------------------------------

%% @doc Webmachine resource that handles document creation.

-module(bb_web_resource_create_condition_report).
-export([init/1,
        to_html/2,
        allowed_methods/2,
        process_post/2,
        is_authorized/2]).
-include_lib("webmachine/include/webmachine.hrl").

init(_Config) ->
    %%{{trace, "/tmp"}, Config}. %% debugging code.
    {ok, no_state}.          %% regular code.

allowed_methods(ReqData, Context)
    -> {['POST'], ReqData, Context}.

is_authorized(ReqData, Context) ->
    ReqBody = wrq:req_body(ReqData),
    JsonReqBody = bb_json:from_json(ReqBody),
    Params = dict:from_list(JsonReqBody),
    Username = dict:fetch(username, Params),
    Password = dict:fetch(password, Params),
    {return_query, ok, IsAuthorized} = bb_database_server:is_user_password_valid(Username, Password),
    case IsAuthorized of
        true ->            
            {IsAuthorized, ReqData, Context};
        _ ->
            Result = [{operation_result, error},
                     {error_string, "Username or password is invalid."}],            
            NewReqData = wrq:append_to_response_body(io_lib:format("~s", [bb_json:to_json(Result)]), ReqData),
            {{halt, 401}, NewReqData, Context}
    end.
    
process_post(ReqData, Context) ->
    ReqBody = wrq:req_body(ReqData),
    JsonReqBody = bb_json:from_json(ReqBody),
    Params = dict:from_list(JsonReqBody),
    Username = dict:fetch(username, Params),
    DocumentBlob = dict:fetch(document_blob, Params),
    JsonDocumentBlob = bb_json:to_json(DocumentBlob),
    {return_query, ok} = bb_database_server:create_condition_report(Username, JsonDocumentBlob),    
    io:format("JsonDocumentBlob: ~p~n", [JsonDocumentBlob]),        
    
    %%NewRd = wrq:append_to_response_body(io_lib:format("<html>testing append.  ReqData: ~p</html>", [ReqData]), ReqData),
    
    NewReqData = wrq:append_to_response_body(io_lib:format("<html>testing append.~n~n", []), ReqData),
    {true, NewReqData, Context}.

to_html(ReqData, Context) ->
    {"<html><head><title>Webmachine File Upload Example</title></head>"
     "<body><h1>Webmachine File Upload Example</h1>"
     "<form action='/' method='POST' enctype='multipart/form-data'>"
     "Upload File:&nbsp<input type='file' name='filedata'/>"
     "<input type='submit' value='Upload'></form></body></html>", 
     ReqData, Context}.

get_streamed_body(done_parts, FileName, Acc) ->
    Bin = iolist_to_binary(lists:reverse(Acc)),
    {FileName, size(Bin)/1024.0, Bin};
get_streamed_body({{"filedata", {Params, _Hdrs}, Content}, Next}, Props, Acc) ->
    FileName = binary_to_list(proplists:get_value(<<"filename">>, Params)),
    get_streamed_body(Next(),[FileName|Props],[Content|Acc]).

