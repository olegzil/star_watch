-module(star_watch_handler).
-behavior(cowboy_handler).

-export([init/2]).
-include("include/apod_record_def.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-import(utils, [date_to_gregorian_days/1, gregorian_days_to_binary/1]).
init(Req0, State) ->
    handle(Req0, State).


handle(Req, State) -> 
  case cowboy_req:method(Req) of
    <<"GET">> -> 
      Body = cowboy_req:has_body(Req),
      Request = reply(get, Body, Req),
        {ok, Request, State}
  end.

  reply(get, _Body, Req) -> 
    submit_request_for_processing(Req).

submit_request_for_processing(Request) ->
    try #{
        start_date  := StartDate,
        end_date    := EndDate
    } = cowboy_req:match_qs([start_date, end_date, api_key], Request) of
         _ ->
            Start = date_to_gregorian_days(StartDate),
            End = date_to_gregorian_days(EndDate),
            Response = supervisor:start_child(star_watch_server_sup, [Start, End]),
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
    
