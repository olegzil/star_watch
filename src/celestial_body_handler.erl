% @Author: Oleg Zilberman
% @Date:   2023-01-11 19:18:26
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-12 11:45:23
-module(celestial_body_handler).
-behavior(cowboy_handler).
-include("include/macro_definitions.hrl").
-import(utils, [date_to_gregorian_days/1, gregorian_days_to_binary/1]).
-export([init/2]).

init(Req0, State) ->
    handle(Req0, State).


handle(Req, State) -> 
  case cowboy_req:method(Req) of
    <<"GET">> -> 
      Body = cowboy_req:has_body(Req),
      Request = submit_request_for_processing(Body, Req),
        {ok, Request, State};
    _ ->
      {error, Req, State}
  end.

submit_request_for_processing(_Body, Request) ->
    try #{
        start_date  := StartDate,
        end_date    := EndDate,
        celestial_object_table := CelestialObject
    } = cowboy_req:match_qs([celstial_object, start_date, end_date, api_key], Request) of
         _ ->
            Start = date_to_gregorian_days(StartDate),
            End = date_to_gregorian_days(EndDate),
            Response = supervisor:start_child(star_watch_server_sup, ?CHILD_SPEC_1(CelestialObject, Start, End)),
            {_, Pid} = Response,
            if 
              is_pid(Pid) -> 
                case gen_server:call(Pid, {fetchdata}, infinity) of
                {ok, Good} ->
                    cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Good, Request),
                    Good;
                  {error, Bad} ->
                    cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Bad, Request),
                    Bad;
                  {_, Other} ->
                    Other
                end,
                gen_server:call(Pid, stop);
              true -> ok
            end
     catch
         _:Error ->
            {_, {_, Term}, _} = Error,
            
            jiffy:encode(Term)     
    end.
    
