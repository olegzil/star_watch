-module(user_login_handler).
-behaviour(cowboy_handler).
-include("include/macro_definitions.hrl").
-export([init/2]).

init(Req0, State) ->
    handle(Req0, State).

handle(Req, State) -> 
  case cowboy_req:method(Req) of
    <<"POST">> -> 
      Request = submit_request_for_processing(Req),
        {ok, Request, State};
    _ ->
      {error, Req, State}
  end.

submit_request_for_processing(Request) ->
    case validate_request(all, Request) of
        {error, Message} ->
            cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Message, Request),
            Message;
        {ok, {ClientID, Parameter}} ->
            Response = star_watch_master_sup:attach_child(login_server, {}),
            Pid = utils:select_pid(Response),
            if
            	is_pid(Pid) ->
		            case execute_request(<<"login">>, ClientID, Parameter) of
		            {ok, Good} ->
		                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Good, Request),
		                Good;
		              {error, Bad} ->
		                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Bad, Request),
		                Bad;
		              {_, Other} ->
		                Other
		            end;
		        true -> utils:log_message(["failed to start server", login_server])
	        end
    end.
execute_request(Action, ClientID, Parameter) ->
	% TODO: Add login_new, login_recover_id, login_recover_password
case Action of
    <<"login">> ->
        gen_server:call(login_server, {login_existing, ClientID, Parameter}, infinity);
    _ ->
        {error, Message} = utils:format_error(?SERVER_ERROR_INVALID_ACTION, <<"invalid action: ", Action/binary>>),
        {error, jiffy:encode(Message)}
end.

validate_request(all, Request) ->
    KeyValidation = validate_request(key, Request),
    ClientIDValidation = validate_request(client_id, Request),
    EncryptedValidation = validate_request(login_payload, Request),
    TestList = [KeyValidation, ClientIDValidation, EncryptedValidation],
    Found = lists:keyfind(error, 1, TestList),
    case Found  of
        false -> %%% no errors found.
            {ok, ClientID} = ClientIDValidation,
            {ok, EncryptedData} = EncryptedValidation,
            {ok, {ClientID, EncryptedData}}; 
        {error, Message} ->

            {error, Message}
    end;

validate_request(login_payload, Request) ->
    case cowboy_req:read_body(Request) of 
        {ok, Data, Request} ->
            {ok, Data};
        FullRequest -> 
            if
                tuple_size(FullRequest) =:= 3 ->
                    {_, Data, _} = FullRequest,
                    {ok, Data};
                true ->
                    {error, Message}  = utils:format_error(?SERVER_ERROR_USER_LOGIN, <<"missing user id and password">>),
                    {error, jiffy:encode(Message)}
            end
        end;
        
validate_request(key, Request) ->
    TokenList = cowboy_req:parse_qs(Request),
    case lists:keyfind(?REQUIRED_CLIENT_KEY_TOKEN, 1, TokenList) of 
        false -> 
            {error, Message}  = utils:format_error(?SERVER_ERROR_MISSING_CLIENT, <<"<key=your client key>">>),
            {error, jiffy:encode(Message)};
        {_, Key} ->        
            if
                Key =/= ?CLIENT_ACCESS_KEY ->
                {error, Message}  = utils:format_error(?SERVER_ERROR_INVALID_CLIENT, <<"default key: ", Key/binary, " is not valid">>),
                {error, jiffy:encode(Message)};                    
            true ->
                {ok, Key}
            end
        end;

validate_request(client_id, Request) ->
    TokenList = cowboy_req:parse_qs(Request),
    case lists:keyfind(?REQUIRED_CLIENT_ID_TOKEN, 1, TokenList) of 
        false -> 
            {error, Message}  = utils:format_error(?SERVER_ERROR_MISSING_CLIENT, <<"<client_key=your client id. NOT your client key>">>),
            {error, jiffy:encode(Message)};
        {_, Key} ->        
            {ok, Key}
        end.

