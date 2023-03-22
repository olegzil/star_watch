% @Author: Oleg Zilberman
% @Date:   2023-02-23 15:56:46
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-21 13:57:30

-module(youtube_channel_directory_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-include("include/macro_definitions.hrl").
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
    try #{
        action := Action,
        key := ClientKey
    } = cowboy_req:match_qs([key, action], Request) of
         _ ->
            case validate_request(Request, ClientKey, Action) of
            {ok, Good} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Good, Request),
                Good;
              {error, Bad} ->
                cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Bad, Request),
                Bad;
              {_, Other} ->
                Other
            end
     catch
         _:Error ->
            {_, {_, Term}, _} = Error,
            io:format("Error: ~p~n", [Error]),          
            jiffy:encode(Term)     
    end.

validate_request(Request, ClientKey, Action) ->
case Action of
     <<"fetch">> ->
        try #{
                channel_name := ChannelName
            } = cowboy_req:match_qs([channel_name], Request) of
            _ ->
                io:format("Action: ~p~nClientKey: ~p~nChannel name: ~p~n", [Action, ClientKey, ChannelName]),
                gen_server:call(db_access_server, {fetchchannelvideos, ClientKey}, infinity)
        catch
         _:Error ->
            {_, {_, Term}, _} = Error,
            {error, jiffy:encode(#{error => jiffy:encode(Term)})}
        end;

        <<"addyoutbechannel">> ->
        gen_server:call(db_access_server, {addyoutubechannel, ClientKey}, infinity);
    _ ->
        utils:format_error(<<"Unrecognized Action: ">>, Action)
end.