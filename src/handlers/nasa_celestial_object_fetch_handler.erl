% @Author: Oleg Zilberman
% @Date:   2023-01-25 19:09:30
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-31 12:07:21
-module(nasa_celestial_object_fetch_handler).
-behavior(cowboy_handler).

-export([init/2]).
-import(utils, [date_to_gregorian_days/1]).

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
        start_date  := StartDate,
        end_date    := EndDate,
        celestial_object := ObjectKey
    } = cowboy_req:match_qs([celestial_object, start_date, end_date, api_key], Request) of
         _ ->
            CelestialObject = string:lowercase(ObjectKey),
            Start = date_to_gregorian_days(StartDate),
            End = date_to_gregorian_days(EndDate),
            Response = process_request(CelestialObject, Start, End),
            cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Response, Request)
     catch
         _:Error ->
            {_, {_, Term}, _} = Error,
            io:format("***** Error *****: ~p~n", [Term]),
            jiffy:encode(Term)     
    end.


process_request(CelestialObject, StartDate, EndDate) ->
			{_Code, Result} = db_access:get_data_for_date_range(nasa, CelestialObject, StartDate, EndDate),
            case Result of
                [] ->
                    jiffy:encode(
                        #{
                            result => error,
                            message => list_to_binary(io_lib:format("No data found for Request: ~pStartDate: ~p EndDate: ~p",[CelestialObject, StartDate, EndDate]))
                        }
                    );  
                _ ->
                    Result
            end.

