% @Author: Oleg Zilberman
% @Date:   2022-10-16 12:45:37
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-11-25 21:43:32
-module(telemetry_handler).
-behavior(cowboy_handler).

-export([init/2]).
-include("include/apodtelemetry.hrl").
init(Req0, State) ->
    Req = handle(Req0, State),
    {ok, Req, State}.


handle(Req, State) -> 
  case cowboy_req:method(Req) of
    <<"GET">> -> 
      #{id := Id} = cowboy_req:match_qs([{id, [], undefined}], Req),
      Request = getMethod(get, Id, Req),
        {ok, Request, State};
    _ ->
        {error, Req, State}
  end.

  getMethod(get, _Id, Req) -> 
    case parse_request(Req) of
            {telemetry, Result} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Result, Req);
            {not_found, Error} ->
                cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Error, Req)
        end.
%%% 
parse_request(Request) ->
    try #{
        uuid := Uuid,
        action := Action
    } = cowboy_req:match_qs([uuid, action], Request) of
        _ -> 
            Result = utils:update_client_record({Uuid, binary_to_atom(Action)}),
            {telemetry, jiffy:encode(#{
                    <<"date_time">> => utils:current_time_string(),
                    <<"status">> => 200,
                    <<"error_text">> => Result
                })}
    catch
        _:Error ->
            io:format("Error: ~p~n", [Error]),
            {_, {_, Term}, _} = Error,
            
            jiffy:encode(Term)             
    end.