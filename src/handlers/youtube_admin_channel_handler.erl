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
            validate_client_id_for_action(<<"deleteconfigrecord">>, TokenList);

        <<"fetchprofilemap">> ->
            {ok, <<"fetchprofilemap">>};

        <<"fetchclientprofile">> ->
            validate_client_id_for_action(<<"fetchclientprofile">>, TokenList);

        <<"updateclientprofile">> ->
            TargetIDParam = lists:keyfind(<<"target_client_id">>, 1, TokenList),
            ChannelParam = lists:keyfind(<<"channel_descipion">>, 1, TokenList),
            VideoParam = lists:keyfind(<<"video_link">>, 1, TokenList),
            if
                TargetIDParam =:= false ->
                    {error, target_client_id_required};
                ChannelParam =:= false ->
                    {error, channel_data_required};
                true ->
                    {_, TargetID} = TargetIDParam,
                    {_, ChannelData} = ChannelParam, % {name, channel_id}
                    {_, VideoLink}   = VideoParam,
                    NameAndID = validate_channel_data(ChannelData),
                    if
                        NameAndID =:= false ->
                            {error, invalid_channel_data};
                        true ->
                          {<<"updateclientprofile">>,  {ClientID, {ChannelName, ChannelID}}} =
                          {<<"updateclientprofile">>, {TargetID, NameAndID}},
                          {ok, {<<"updateclientprofile">>,  {ClientID, {ChannelName, ChannelID, VideoLink}}}}
                    end
            end;
        _ ->
            {error, no_such_command}
    end.
validate_channel_data(ChannelData) ->
    Data = string:split(ChannelData, ",", all),
    Count = length(Data), 
    case Count of
        2 ->
            {lists:nth(1, Data), lists:nth(2, Data)};
        _ ->
            false
    end.

validate_client_id_for_action(Action, TokenList) ->
    Result = lists:keyfind(<<"client_id">>, 1, TokenList),
    case Result of
        {<<"client_id">>, ClientID} ->
            Found = server_config_processor:is_client_in_profile_map(approved, ClientID),
            if
                Found =:= false ->
                    {error, no_such_client};
                true ->
                    {ok, {Action, ClientID}}
            end;
        _ ->
            {error, invalid_parameters}
    end.

validate_admin_key(Key) when ?ADMINISTRATOR_KEY =:= Key -> ok;
validate_admin_key(_Arg) -> error.

