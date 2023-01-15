% @Author: Oleg Zilberman
% @Date:   2023-01-11 19:18:26
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-13 20:21:03
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

submit_request_for_processing(Body, Request) ->
    try #{
        year_start  := StartDate,
        year_end    := EndDate,
        celestial_object := CelestialObject
    } = cowboy_req:match_qs([celestial_object, year_start, year_end, api_key], Request) of
         _ ->
            Start = date_to_gregorian_days(StartDate),
            End = date_to_gregorian_days(EndDate),
            io:format("Received: Start=~p  End=~p~n", [Start, End]),
            Response = supervisor:start_child(star_watch_master_sup, [CelestialObject, Start, End]),
            io:format("supervisor:start_child returned ~p~n", [Response]),
            {_, Pid} = Response,
            if 
              is_pid(Pid) -> 
                case gen_server:call(Pid, {fetchnasadata}, infinity) of
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
            io:format("***** Error *****: ~p~n", [Term]),
            jiffy:encode(Term)     
    end.
    
