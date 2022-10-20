% @Author: Oleg Zilberman
% @Date:   2022-10-16 12:45:37
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-10-16 17:45:06
-module(telemetry_handler).
-behavior(cowboy_handler).

-export([init/2]).
-include("include/apodtelemetry.hrl").
init(Req0, State) ->
    Req = handle(Req0, State),
    {ok, Req, State}.


handle(Req, State) -> 
  case cowboy_req:method(Req) of
    <<"POST">> -> 
      Body = cowboy_req:has_body(Req),
      Request = postMethod(<<"POST">>, Body, Req),
        {ok, Request, State};
    <<"GET">> -> 
      #{id := Id} = cowboy_req:match_qs([{id, [], undefined}], Req),
      Request = getMethod(<<"GET">>, Id, Req),
        {ok, Request, State};
    <<"PUT">> -> 
      Body = cowboy_req:has_body(Req),
      Request = putMethod(<<"PUT">>, Body, Req),
        {ok, Request, State}
  end.

  postMethod(<<"POST">>, _Body, Req) -> 
    case parse_request(Req) of
            {telemetry, Result} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Result, Req);
            {not_found, Error} ->
                cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Error, Req)
        end.

  getMethod(<<"GET">>, _Id, Req) -> 
    case parse_request(Req) of
            {telemetry, Result} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Result, Req);
            {not_found, Error} ->
                cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Error, Req)
        end.

  putMethod(<<"PUT">>, _Body, Req) -> 
    case parse_request(Req) of
            {telemetry, Result} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Result, Req);

            {not_found, Error} ->
                cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Error, Req)
        end.


%%% 
parse_request(Request) ->
    #{headers := Headers} = Request,
    io:format("~p~n", [utils:update_client_record(maps:find(<<"host">>, Headers))]),
    {telemetry, jiffy:encode(#{
            <<"date_time">> => utils:current_time_string(),
            <<"status">> => 200,
            <<"error_text">> => <<"success">>
        })}.
    