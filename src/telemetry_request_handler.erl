% @Author: Oleg Zilberman
% @Date:   2022-11-01 12:57:44
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-15 14:00:34
-module(telemetry_request_handler).
-behavior(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Req = handle(Req0, State),
    {ok, Req, State}.

handle(Req, State) -> 
    case cowboy_req:method(Req) of
    <<"GET">> -> 
      #{api_key := Id} = cowboy_req:match_qs([{api_key, [], undefined}], Req),
      Request = reply(get, Id, Req),
        {ok, Request, State};
    _ ->
        {error, Req, State}
    end.

  reply(get, _Id, Request) -> 
    try #{
        action  := Action
    } = cowboy_req:match_qs([action], Request) of
         _ ->
         	ActionSelector = binary_to_atom(Action),
         	execute_action(ActionSelector, Request)
     catch
         _:Error ->
            {_, {_, Term}, _} = Error,
            
            jiffy:encode(Term)     
    end.

execute_action(fetchall, Request) ->
	Start = utils:date_to_gregorian_days(<<"1970-01-01">>),
	{{Year, Month, Day}, {_A, _B, _C}} = calendar:now_to_datetime(erlang:timestamp()),
	End = calendar:date_to_gregorian_days({Year, Month, Day}),
    Response = supervisor:start_child(star_watch_master_sup, [Start, End]),
    {_, Pid} = Response,
    if 
      is_pid(Pid) -> 
        case gen_server:call(Pid, {fetchdata}, infinity) of
        {ok, Good} ->
            cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Good, Request),
            Good;
          {error, Bad} ->
            cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, [jiffy:encode(Bad)], Request),
            Bad;
          {_, Other} ->
            Other
        end,
        gen_server:call(Pid, stop);
      true -> ok
    end;
execute_action(datasetsize, Request) ->
    try #{
        start_date  := StartDate,
        end_date  := EndDate
    } = cowboy_req:match_qs([start_date, end_date], Request) of
         _ ->
          fetch_dataset_size(StartDate, EndDate, Request)
     catch
         _:Error ->
            {_, {_, Term}, _} = Error,
            
            jiffy:encode(Term)     
    end.

fetch_dataset_size(StartDate, EndDate, Request) ->
  Start = utils:date_to_gregorian_days(StartDate),
  End = utils:date_to_gregorian_days(EndDate),
    Response = star_watch_master_sup:attach_child(apod, {Start, End}),
    {_, Pid} = Response,
    if 
      is_pid(Pid) -> 
        case gen_server:call(Pid, {datasetsize}, infinity) of
        {ok, Good} ->
            cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Good, Request),
            Good;
          {error, Bad} ->
            cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, [jiffy:encode(Bad)], Request),
            Bad;
          {_, Other} ->
            Other
        end,
        gen_server:call(Pid, stop);
      true -> ok
    end.

