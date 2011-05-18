-module(common_resource_functions).
-export([is_authorized/2,
        get_json_dict/1]).

is_authorized(ReqData, Context) ->
    Params = get_json_dict(ReqData),
    Username = dict:fetch(username, Params),
    Password = dict:fetch(password, Params),
    {return_query, ok, IsAuthorized} = bb_database_server:is_user_password_valid(Username, Password),
    case IsAuthorized of
        true ->            
            {true, ReqData, Context};
        _ ->
            Result = [{operation_result, error},
                     {error_string, "Username or password is invalid."}],            
            NewReqData = wrq:append_to_response_body(io_lib:format("~s", [bb_json:to_json(Result)]), ReqData),
            {{halt, 401}, NewReqData, Context}
    end.
    
get_json_dict(ReqData) ->
    ReqBody = wrq:req_body(ReqData),
    JsonReqBody = bb_json:from_json(ReqBody),
    dict:from_list(JsonReqBody).
    