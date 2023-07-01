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
    <<"GET">> ->   
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
        {ok, {ClientID, Action, Parameter}} ->
            Response = star_watch_master_sup:attach_child(login_server, {}),
            Pid = utils:select_pid(Response),
            if
            	is_pid(Pid) ->
		            case execute_request(Action, ClientID, Parameter) of
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
    case Action of
        <<"login_new_password">> ->
            Result = gen_server:call(login_server, {login_new_password, ClientID, Parameter}, infinity),
            case Result of
                {ok, Good} ->
                    {ok, jiffy:encode(#{success => Good})};
                {error, Bad} ->
                    {error, jiffy:encode(#{error =>Bad})}
            end;

        <<"login_new_userid">> ->
            Result = gen_server:call(login_server, {login_new_userid, ClientID, Parameter}, infinity),
            case Result of
                {ok, Good} ->
                    {ok, jiffy:encode(#{success => Good})};
                {error, Bad} ->
                    {error, jiffy:encode(#{error =>Bad})}
            end;

        <<"clear_login_table">> ->
            Result = gen_server:call(login_server, {clear_login_table}, infinity),
            case Result of
                {ok, Good} ->
                    {ok, jiffy:encode(#{success => Good})};
                {error, Bad} ->
                    {error, jiffy:encode(#{error =>Bad})};
                _ ->
                    {ok, jiffy:encode(#{success => <<"login table cleared">>})}
            end;
        _ ->
            {error, Message} = utils:format_error(?SERVER_ERROR_INVALID_ACTION, <<"invalid action: ", Action/binary>>),
            {error, jiffy:encode(Message)}
    end.

validate_request(all, Request) ->
    KeyValidation = validate_request(key, Request),
    ClientIDValidation = validate_request(client_id, Request),
    EncryptedValidation = validate_request(login_payload, Request),
    ActionValidation = validate_request(action, Request),
    TestList = [KeyValidation, ClientIDValidation, EncryptedValidation, ActionValidation],
    Found = lists:keyfind(error, 1, TestList),
    case Found  of
        false -> %%% no errors found.
            {ok, ClientID} = ClientIDValidation,
            {ok, EncryptedData} = EncryptedValidation,
            {ok, Action} = ActionValidation,
            {ok, {ClientID, Action, EncryptedData}}; 
        {error, Message} ->

            {error, Message}
    end;
validate_request(action, Request) ->
    TokenList = cowboy_req:parse_qs(Request),
    case lists:keyfind(?REQUIRED_ACTION_TOKEN, 1, TokenList) of 
        false -> 
            HelpMessage = lists:foldl(fun(Item, Acc) ->
                Token = <<Acc/binary," ", Item/binary>>,
                Token
             end, <<"missing action. action must be one of ">>, ?AVAILABLE_CHANNEL_ACTIONS),
            {error, Message}  = utils:format_error(?SERVER_ERROR_INVALID_ACTION, HelpMessage),
            {error, jiffy:encode(Message)};
        {_, Action} ->      
            Found = lists:member(Action, ?AVAILABLE_CHANNEL_ACTIONS), 
            if Found =:= true->
                    {ok, Action};
                true ->
                    {error, Message}  = utils:format_error(?SERVER_ERROR_INVALID_ACTION, <<"no such action: ", Action/binary>>),
                    {error, jiffy:encode(Message)}
            end
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

