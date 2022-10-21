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
    <<"POST">> -> 
      Body = cowboy_req:has_body(Req),
      Request = reply(post, Body, Req),
        {ok, Request, State};
    <<"GET">> -> 
      #{id := Id} = cowboy_req:match_qs([{id, [], undefined}], Req),
      Request = reply(get, Id, Req),
        {ok, Request, State};
    <<"PUT">> -> 
      Body = cowboy_req:has_body(Req),
      Request = reply(put, Body, Req),
        {ok, Request, State}
  end.

  reply(post, _Body, Req) -> 
    submit_request_for_processing(Req);

  reply(get, _Id, Req) -> 
    submit_request_for_processing(Req);

  reply(put, _Body, Req) -> 
    submit_request_for_processing(Req).
        
submit_request_for_processing(Request) ->
    try #{
        start_date  := StartDate,
        end_date    := EndDate
    } = cowboy_req:match_qs([start_date, end_date, api_key], Request) of
         _ ->
            Start = date_to_gregorian_days(StartDate),
            End = date_to_gregorian_days(EndDate),
            timer:sleep(100),
            Response = ppool:sync_queue(database_server, [Start, End, Request]),
            timer:sleep(100),
            {_, Pid} = Response,
            if 
              is_pid(Pid) -> 
                gen_server:call(Pid, {saysomething, <<"Well isn't this something.">>}, infinity),
                Pid ! stop; % Kill the worker as soon as it has returned its data
              true -> ok
            end
     catch
         _:Error ->
            {_, {_, Term}, _} = Error,
            
            jiffy:encode(Term)     
    end.
    
