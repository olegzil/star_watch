% @Author: Oleg Zilberman
% @Date:   2023-03-08 18:51:44
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-29 21:08:29
-module(youtube_admin_channel_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-include("include/macro_definitions.hrl").
-import(utils, [date_to_gregorian_days/1, gregorian_days_to_binary/1]).
init(Req0, State) ->
    handle(Req0, State).

handle(Req, State) -> 
  case cowboy_req:method(Req) of
    <<"GET">> -> 
      Request = submit_request_for_processing(Req),
        {ok, Request, State};
    _ ->
      {error, Req, State}
  end.

submit_request_for_processing(Request) ->
    TokenList = cowboy_req:parse_qs(Request),
    ValidationResult = validate_request(all, TokenList),    

    case ValidationResult of
        {ok, Result} ->
            case administrator:handle_admin_action(Result) of
            {ok, Good} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, jiffy:encode(Good), Request),
                Good;
              {error, Bad} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, jiffy:encode(Bad), Request),
                Bad;
              {_, Other} ->
                Other
            end;
        {error, Bad} ->
            cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, jiffy:encode(Bad), Request)
    end.
validate_request(all, TokenList) ->
    ValidateAdminKey =  validate_request(adminkey, TokenList),
    AdminAction = validate_request(action, TokenList),

    TestList = [ValidateAdminKey, AdminAction],
    ErrorTuple = lists:keyfind(error, 1, TestList),
    case ErrorTuple of
        false ->
            AdminAction;            
        {error, Error} ->
            {ErrorName, ErrorCode} = lists:keyfind(Error, 1, ?RESPONSE_CODES),
            utils:format_error(ErrorCode, ErrorName)
    end;

validate_request(adminkey, TokenList) ->
    Target = lists:keyfind(<<"key">>, 1, TokenList),
    if
        Target =:= false ->
            {error, no_admin_key};
        true ->
            {_Tag, AdminKey} = Target,
            validate_admin_key(AdminKey)
    end;
validate_request(action, TokenList) ->
    Target = lists:keyfind(<<"action">>, 1, TokenList),
    if
        Target =:= false ->
            {error, no_action_specified};
        true ->
            case Target of
                {_Tag, Action} ->    
                    validate_action(Action, TokenList);
                _ ->
                    {error, missing_action_type}                
            end
    end.

validate_action(Action, TokenList) ->
    case Action of
        <<"deleteconfigrecord">> -> % Format: action=deleteconfigrecord&client_id=<ClientID>
            Result = lists:keyfind(<<"client_id">>, 1, TokenList),
            case Result of
                {<<"client_id">>, ClientID} ->
                    Found = server_config_processor:is_client_in_profile_map(ClientID),
                    if
                        Found =:= false ->
                            {error, no_such_client};
                        true ->
                            {ok, {<<"deleteconfigrecord">>, ClientID}}
                    end;
                _ ->
                    {error, invalid_parameters}
            end;

        <<"deleteyoutubechannel">> -> % Format: {action,deleteyoutubechannel}, {channel_id, <Channel id>}, {client_id, <Client id>}
            ChannelParam = lists:keyfind(<<"channel_id">>, 1, TokenList),
            ClientParam = lists:keyfind(<<"client_id">>, 1, TokenList),
            WellFormed = if
                ChannelParam =:= false ->
                    {error, channel_id_required};
                ClientParam =:= false ->
                    {error, client_id_required};
                true ->
                    {_, ChannelID} = ChannelParam,
                    {_, ClientID} = ClientParam,
                    case server_config_processor:is_channel_in_profile(ClientID, ChannelID) of
                        false ->
                            {error, no_such_channel};
                        true ->
                            {ok, ClientParam, ChannelParam}
                    end
            end,
            case WellFormed of
                {error, Error} ->
                    {error, Error};
                {ok, ClientParam, ChannelParam}->
                    {<<"channel_id">>, Channel} = ChannelParam,
                    {<<"client_id">>, Client} = ClientParam,
                    Target = server_config_processor:is_client_in_profile_map(Client),
                    if 
                        Target =:= true ->
                            {ok, { <<"deleteyoutubechannel">>, {Client, Channel}}};
                        true ->
                            {error, no_such_client}
                    end
            end;
            
        <<"promoteconfigrecord">> ->  % Format: {action, promoteconfigrecord}, {client_id, <Client id>}
            {error, not_implemented};
        <<"fetchprofilemap">> ->
            {error, not_implemented};
        <<"fetchclientconfigdata">> ->
            {error, not_implemented}
    end.

validate_admin_key(Key) when ?ADMINISTRATOR_KEY =:= Key -> ok;
validate_admin_key(_Arg) -> error.

