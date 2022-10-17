-module(star_watch_handler).
-behavior(cowboy_handler).

-export([init/2, process_date_request/3]).
-include("include/apod_record_def.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-import(utils, [date_to_gregorian_days/1, gregorian_days_to_binary/1]).
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
            {ok, Result} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Result, Req);
            {not_found, Error} ->
                cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Error, Req)
        end.

  getMethod(<<"GET">>, _Id, Req) -> 
    case parse_request(Req) of
            {ok, Result} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Result, Req);
            {not_found, Error} ->
                cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Error, Req)
        end.

  putMethod(<<"PUT">>, _Body, Req) -> 
    case parse_request(Req) of
            {ok, Result} ->
                cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Result, Req);
            {not_found, Error} ->
                cowboy_req:reply(404,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Error, Req)
        end.


%%% 
parse_request(Request) ->
    #{headers := Headers} = Request,
    utils:update_client_record(maps:find(<<"host">>, Headers)),
    try #{
        start_date  := StartDate,
        end_date    := EndDate
    } = cowboy_req:match_qs([start_date, end_date, api_key], Request) of
         _ ->
            process_date_request(date_to_gregorian_days(StartDate), date_to_gregorian_days(EndDate), Request)
     catch
         _:Error ->
            {_, {_, Term}, _} = Error,
            
            jiffy:encode(Term)     
    end.
    
process_date_request(StartDate, EndDate, _Req) ->
    Match = ets:fun2ms(
        fun(Record) 
            when Record#apodimagetable.date >= StartDate, 
                 Record#apodimagetable.date =< EndDate ->
                Record
        end),

    SelectRecords = fun() -> mnesia:select(apodimagetable, Match) end,
    {_, ListOfRecords} = mnesia:transaction(SelectRecords),
    case length(ListOfRecords) of
        0 ->
            io:format("not found on db. fetching from NASA~n"),
            case utils:fetch_apod_data(StartDate, EndDate, notfound) of
                {error, _} ->
                    io:format("NASA fetch failed~n"),
                    date_rage_not_found(StartDate, EndDate);
                {ok, JsonResult} ->
                    io:format("NASA fetch succeeded~n"),
                    {ok, JsonResult}
            end;

        _ ->
            io:format("Found on local db~n"),
            JsonFreindly = lists:map(fun(DbItem) ->
                                #{url => DbItem#apodimagetable.url,
                                             copyright => DbItem#apodimagetable.copyright,
                                             date => gregorian_days_to_binary(DbItem#apodimagetable.date),
                                             explanation => DbItem#apodimagetable.explanation,
                                             hdurl => DbItem#apodimagetable.hdurl,
                                             media_type => DbItem#apodimagetable.media_type,
                                             service_version => DbItem#apodimagetable.service_version,
                                             title => DbItem#apodimagetable.title}                                 
                                         end, ListOfRecords),
            io:format("Reqeust: ~p~n", [jiffy:encode([gregorian_days_to_binary(StartDate), gregorian_days_to_binary(EndDate)])]),
            io:format("Rerturn: ~p records~n", [length(JsonFreindly)]),
            {ok, jiffy:encode(JsonFreindly)}
    end.

date_rage_not_found(StartDate, EndDate) ->
    DateStart = gregorian_days_to_binary(StartDate),
    DateEnd = gregorian_days_to_binary(EndDate),
    Message = <<"Date range not found: ">>,
    ErrorResponse = #{
        <<"date_time">> => utils:current_time_string(),
        <<"error_code">> => 404,
        <<"error_text">> => <<Message/binary, DateStart/binary, <<" -- ">>/binary, DateEnd/binary>>
    },
    {not_found, jiffy:encode(ErrorResponse)}.   

