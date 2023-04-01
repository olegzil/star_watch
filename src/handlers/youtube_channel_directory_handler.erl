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

%%% Required values: action = fetchchanneldirectory | fetchchannelvideos
%%% If client_id is specified, the value represents custom youtube channel directory, the default directory otherwise.
submit_request_for_processing(Request) ->
    case validate_request(all, Request) of
        {error, Message} ->
            cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Message, Request),
            Message;
        {ok, {Action, ClientID, ChannelID}} ->
            case execute_request(Action, ClientID, ChannelID) of
            {ok, Good} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Good, Request),
                Good;
              {error, Bad} ->
                cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, jiffy:encode(Bad), Request),
                Bad;
              {_, Other} ->
                Other
            end
    end.

%%% TODO: execute_request should call validate_request(), because validation will differ depending on Action
execute_request(Action, ClientID, ChannelID) ->
case Action of
     <<"fetchchannelvideos">> ->
        gen_server:call(db_access_server, {fetchchannelvideos, ChannelID}, infinity);
    <<"fetchchanneldirectory">> ->
        gen_server:call(db_access_server, {fetchchanneldirectory, ClientID}, infinity);
    _ ->
        utils:format_error(<<"Unrecognized Action: ">>, Action)
end.

validate_channel_id(Request) ->
    TokenList = cowboy_req:parse_qs(Request),
    case lists:keyfind(?REQUIRED_CHANNEL_ID_TOKEN, 1, TokenList) of 
        false ->
            {error, Message}  = utils:format_error(<<"missing channel id token ">>, <<"channel_id=<your channel id>">>),
            {error, jiffy:encode(Message)};
        {_, ChannelID} ->
            {ok, ChannelID}
    end.

validate_request(all, Request) ->
    KeyValidation = validate_request(key, Request),
    ActionValidation = validate_request(action, Request),
    ClientKeyValidation = validate_request(client_key, Request),
    ChannelIDValidation = validate_request(channel_id, Request),
    TestList = [KeyValidation, ActionValidation, ClientKeyValidation, ChannelIDValidation],
    Found = lists:keyfind(error, 1, TestList),
    case Found  of
        false -> %%% no errors found.
            {ok, Action} = ActionValidation,
            {ok, ClientID} = ClientKeyValidation,
            {ok, ChannelID} = ChannelIDValidation,
            {ok, {Action, ClientID, ChannelID}};
        {error, Message} ->
            {error, jiffy:encode(Message)}
    end;

validate_request(channel_id, Request) ->
    TokenList = cowboy_req:parse_qs(Request),
    case lists:keyfind(?REQUIRED_ACTION_TOKEN, 1, TokenList) of 
        false ->
            {error, Message}  = utils:format_error(<<"missing channel id token ">>, <<"channel_id=<your channel id>">>),
            {error, jiffy:encode(Message)};
        {_, Action} ->
            if
                Action =:= <<"fetchchannelvideos">> ->
                    validate_channel_id(Request);
                true ->
                    {ok, ignore}
            end
    end;

validate_request(key, Request) ->
    TokenList = cowboy_req:parse_qs(Request),
    case lists:keyfind(?REQUIRED_CLIENT_KEY_TOKEN, 1, TokenList) of 
        false -> 
            {error, Message}  = utils:format_error(<<"missing client key ">>, <<"<key=your client key>">>),
            {error, jiffy:encode(Message)};
        {_, Key} ->        
            if
                Key =/= ?CLIENT_ACCESS_KEY ->
                {error, Message}  = utils:format_error(<<"invalid client key ">>, Key),
                {error, jiffy:encode(Message)};                    
            true ->
                {ok, Key}
            end
        end;
validate_request(action, Request) ->
    TokenList = cowboy_req:parse_qs(Request),
    case lists:keyfind(?REQUIRED_ACTION_TOKEN, 1, TokenList) of 
        false -> 
            {error, Message}  = utils:format_error(<<"missing ">>, <<"action=<fetchchanneldirectory|fetchchannelvideos>">>),
            {error, jiffy:encode(Message)};
        {_, Action} ->      
            Found = lists:member(Action, ?AVAILABLE_CHANNEL_ACTIONS),  
            if Found =:= true ->
                    {ok, Action};
                true ->
                    {error, Message}  = utils:format_error(<<"invalid action ">>, Action),
                    {error, jiffy:encode(Message)}
            end
        end;
validate_request(client_key, Request) ->
    TokenList = cowboy_req:parse_qs(Request),
    case lists:keyfind(?REQUIRED_CLIENT_ID_TOKEN, 1, TokenList) of 
        false -> 
            {ok, ?CLIENT_ACCESS_KEY};
        {_, Key} ->        
            {ok, Key}
        end.
