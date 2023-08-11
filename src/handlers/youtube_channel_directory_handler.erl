% @Author: Oleg Zilberman
% @Date:   2023-02-23 15:56:46
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-30 16:44:47

-module(youtube_channel_directory_handler).
-behaviour(cowboy_handler).
-include("include/macro_definitions.hrl").
-export([init/2]).

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

%%% Required values: action = fetchclientdirectory | fetchchannelvideos
%%% If client_id is specified, the value represents custom youtube channel directory, the default directory otherwise.
submit_request_for_processing(Request) ->
    case validate_request(all, Request) of
        {error, Message} ->
            cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Message, Request),
            Message;
        {ok, {Action, ClientID, Parameter}} ->
            case execute_request(Action, ClientID, Parameter) of
            {ok, Good} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Good, Request),
                Good;
              {error, Bad} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Bad, Request),
                Bad;
              {_, Other} ->
                Other
            end
    end.

execute_request(Action, ClientID, Parameter) ->
case Action of
    <<"restoredefaultclient">> ->
        gen_server:call(db_access_server, {restoredefaultclient, ClientID}, infinity);
    <<"deletechannel">> ->
        gen_server:call(db_access_server, {deletechannel, ClientID, Parameter}, infinity);
     <<"fetchchannelvideos">> ->
        gen_server:call(db_access_server, {fetchchannelvideos, ClientID, Parameter}, infinity);
    <<"fetchclientdirectory">> ->
        gen_server:call(db_access_server, {fetchclientdirectory, ClientID}, infinity);
    <<"updatechannel">> ->
        gen_server:call(db_access_server, {updatechannel, ClientID, Parameter}, infinity);
    <<"addvideolink">> ->
        gen_server:call(db_access_server, {addvideolink, ClientID, Parameter}, infinity);
    <<"deletevideolink">> ->
        gen_server:call(db_access_server, {deletevideolink, ClientID, Parameter}, infinity);
    <<"linkstatus">> ->
        gen_server:call(db_access_server, {linkstatus, ClientID, Parameter}, infinity);
    _ ->
        {error, Message} = utils:format_error(?SERVER_ERROR_INVALID_ACTION, <<"invalid action: ", Action/binary>>),
        {error, jiffy:encode(Message)}
end.

validate_request(all, Request) ->
    KeyValidation = validate_request(key, Request),
    ActionValidation = validate_request(action, Request),
    ClientIDValidation = validate_request(client_id, Request),
    TestList = [KeyValidation, ActionValidation, ClientIDValidation],
    Found = lists:keyfind(error, 1, TestList),
    case Found  of
        false -> %%% no errors found.
            {ok, {Action, Parameter}} = ActionValidation,
            {ok, ClientID} = ClientIDValidation,
            {ok, {Action, ClientID, Parameter}}; %possible values {ok, {ChannelID, ClientID}} | {ok, {ClientID, VideoLink}}
        {error, Message} ->
            {error, Message}
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
                    secondary_action_validation(Action, TokenList);
                true ->
                    {error, Message}  = utils:format_error(?SERVER_ERROR_INVALID_ACTION, <<"no such action2: ", Action/binary>>),
                    {error, jiffy:encode(Message)}
            end
        end.

secondary_action_validation(Action, TokenList) ->
    case Action of
        <<"linkstatus">> ->
            VideoLinkParam = lists:keyfind(<<"video_link">>, 1, TokenList),
            if
                VideoLinkParam =:= false ->
                    {error, Message} = utils:format_error(?SERVER_ERROR_MISSING_VIDEO, missing_video_link),
                    {error, jiffy:encode(Message)};
                true ->

                    {_, Link} = VideoLinkParam,
                    Parts = string:split(Link, "/", all),
                    if 
                        length(Parts) < 4 ->
                            {error, Message} = utils:format_error(?SERVER_ERROR_INVALID_LINK, video_link_wrong_format),
                            {error, jiffy:encode(Message)};
                        true ->
                            {ok, {Action, Link}}                    
                    end
            end;

        <<"addvideolink">> -> %% Returns {error, ErrorMessage} or {ClientID, VideoLink}
            VideoLinkParam = lists:keyfind(<<"video_link">>, 1, TokenList),
            if
                VideoLinkParam =:= false ->
                    {error, Message} = utils:format_error(?SERVER_ERROR_MISSING_VIDEO, missing_video_link),
                    {error, jiffy:encode(Message)};
                true ->

                    {_, Link} = VideoLinkParam,
                    Parts = string:split(Link, "/", all),
                    if 
                        length(Parts) < 4 ->
                            {error, Message} = utils:format_error(?SERVER_ERROR_INVALID_LINK, video_link_wrong_format),
                            {error, jiffy:encode(Message)};
                        true ->
                            {ok, {Action, Link}}                    
                    end
            end;
        <<"deletechannel">> ->
            ChannelParam = lists:keyfind(<<"channel_id">>, 1, TokenList),
            if
                ChannelParam =:= false ->
                    {error, Message} = utils:format_error(?SERVER_ERROR_MISSING_CHANNEL, channel_id_required),
                    {error, jiffy:encode(Message)};
                true ->
                    {_, ChannelID} = ChannelParam,
                    {ok, {Action, ChannelID}}                    
            end;

        <<"deletevideolink">> ->
            VideoLinkParam = lists:keyfind(<<"video_link">>, 1, TokenList),
            if
                VideoLinkParam =:= false ->
                    {error, Message} = utils:format_error(?SERVER_ERROR_MISSING_VIDEO, missing_video_link),
                    {error, jiffy:encode(Message)};
                true ->

                    {_, Link} = VideoLinkParam,
                    Parts = string:split(Link, "/", all),
                    if 
                        length(Parts) < 4 ->
                            {error, Message} = utils:format_error(?SERVER_ERROR_INVALID_LINK, video_link_wrong_format),
                            {error, jiffy:encode(Message)};
                        true ->
                            {ok, {Action, Link}}                    
                    end
            end;

        <<"fetchchannelvideos">> -> %% Returns {error, ErrorMessage} or {ClientID, ignore}
            case lists:keyfind(?REQUIRED_CHANNEL_ID_TOKEN, 1, TokenList) of 
                false ->
                    {error, Message}  = utils:format_error(?SERVER_ERROR_MISSING_CHANNEL, <<"channel_id=<your channel id>">>),
                    {error, jiffy:encode(Message)};
                {_, ChannelID} ->
                    {ok, {Action, ChannelID}}
            end;
        <<"fetchclientdirectory">> ->
            case lists:keyfind(?REQUIRED_CLIENT_ID_TOKEN, 1, TokenList) of 
                false ->
                    {error, Message}  = utils:format_error(?SERVER_ERROR_MISSING_CHANNEL, <<"client_id=<your client id>">>),
                    {error, jiffy:encode(Message)};
                {_, ClientID} ->
                    {ok, {Action, ClientID}}
            end;    
        <<"updatechannel">> ->
            case lists:keyfind(?REQUIRED_CHANNEL_ID_TOKEN, 1, TokenList) of 
                false ->
                    {error, Message}  = utils:format_error(?SERVER_ERROR_MISSING_CHANNEL, <<"channel_id=<your channel id>">>),
                    {error, jiffy:encode(Message)};
                {_, ChannelID} ->
                    {ok, {Action, ChannelID}}
            end;    
        <<"restoredefaultclient">> ->
            case lists:keyfind(?REQUIRED_CLIENT_ID_TOKEN, 1, TokenList) of 
                false ->
                    {error, Message}  = utils:format_error(?SERVER_ERROR_MISSING_CHANNEL, <<"client_id=<your client id>">>),
                    {error, jiffy:encode(Message)};
                {_, ClientID} ->
                    {ok, {Action, ClientID}}
            end;    
        _ ->
            {ok, Action}

    end.
