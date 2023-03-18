% @Author: Oleg Zilberman
% @Date:   2023-02-23 15:56:46
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-17 21:43:53

-module(youtube_channel_directory_handler).
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
    try #{
        key := ClientKey
    } = cowboy_req:match_qs([key], Request) of
         _ ->
            RequestResult = db_access:process_channel_request(?SERVER_CONFIG_FILE, ClientKey),
            case RequestResult of
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
    
