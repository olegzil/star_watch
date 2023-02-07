% @Author: Oleg Zilberman
% @Date:   2023-01-11 19:18:26
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-02-04 12:10:59
-module(nasa_celestial_object_db_populate_handler).
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
        page := Page,
        api_key := Key,
        celestial_object := ObjectKey
    } = cowboy_req:match_qs([{page, [], <<"1">>}, celestial_object, start_date, end_date, api_key], Request) of
         _ ->
            CelestialObject = string:lowercase(ObjectKey),
            io:format("~n******************~nPage: ~p~nObject: ~p~nStart: ~p~nEnd: ~p~nKey: ~p~n", [Page, CelestialObject, StartDate, EndDate, Key]),
            ObjectList = string:split(binary_to_list(CelestialObject), ",", all),
            Start = date_to_gregorian_days(StartDate),
            End = date_to_gregorian_days(EndDate),
            Response = star_watch_master_sup:attach_child(nasageneralapi, {ObjectList, binary_to_integer(Page), Start, End}),
            {_ChildID, {_ChildStartResults, Pid}} = Response,
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
    
