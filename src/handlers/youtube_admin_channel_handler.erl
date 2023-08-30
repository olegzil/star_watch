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
            {Action, Parameter} = Result,
            case administrator:handle_admin_action(Action, Parameter) of
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
    ValidateClientKey = validate_request(clientkey, TokenList),
    AdminAction = validate_request(action, TokenList),

    TestList = [ValidateAdminKey, AdminAction, ValidateClientKey],
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

validate_request(clientkey, TokenList)     ->
    Result = lists:keyfind(<<"client_id">>, 1, TokenList),
    case Result of
        {<<"client_id">>, ClientID} ->
            Found = server_config_processor:is_client_in_profile_map(ClientID),
            if
                Found =:= false ->
                    {error, no_such_client};
                true ->
                    {ok, ClientID}
            end;
        _ ->
            {error, client_id_required}
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
        <<"addchannel">> ->
            {_, ClientID} = lists:keyfind(<<"client_id">>, 1, TokenList),
            case lists:keyfind(<<"channel_id">>, 1, TokenList) of
                false ->
                    {error, channel_id_required};
                {_, ChannelID} ->
                    case lists:keyfind(<<"channel_name">>, 1, TokenList) of
                        false ->
                            {error, channel_name_required};
                        {_, ChannelName} ->
                            {ok, {<<"addchannel">>, {ClientID, ChannelID, ChannelName}}}
                    end
            end;

        <<"deleteconfigrecord">> -> % Format: action=deleteconfigrecord&client_id=<ClientID>
            {_, ClientID} = lists:keyfind(<<"client_id">>, 1, TokenList),
            {ok, {<<"deleteconfigrecord">>, {ClientID, undefined}}}; %% Degenerate case this call has no other parameters

        <<"fetchprofilemap">> ->
            {_, ClientID} = lists:keyfind(<<"client_id">>, 1, TokenList),
            {ok, {<<"fetchprofilemap">>, {ClientID, undefined}}}; %% Degenerate case this call has no other parameters

        <<"fetchclientprofile">> ->
            {_, ClientID} = lists:keyfind(<<"client_id">>, 1, TokenList),
            {ok, {<<"fetchclientprofile">>, {ClientID, undefined}}};  %% Degenerate case this call has no other parameters

        <<"deleteyoutubechannel">> ->
            case lists:keyfind(<<"channel_id">>, 1, TokenList) of
                false ->
                    {error, channel_id_required};
                {_, ChannelID} ->
                    {_, ClientID} = lists:keyfind(<<"client_id">>, 1, TokenList),
                    {ok, {<<"deleteyoutubechannel">>, {ClientID, ChannelID}}};
                _ ->
                    {error, no_such_command}
            end;
        _ ->
            {error, no_such_command}
    end.

validate_admin_key(Key) when ?ADMINISTRATOR_KEY =:= Key -> ok;
validate_admin_key(_Arg) -> error.

