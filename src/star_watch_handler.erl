-module(star_watch_handler).
-behavior(cowboy_handler).

-export([init/2]).
-include("include/apod_record_def.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-import(utils, [date_to_gregorian_days/1, gregorian_days_to_string_date/1]).
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
    cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, parse_request(Req), Req).

  getMethod(<<"GET">>, _Id, Req) -> 
    cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, parse_request(Req), Req).

  putMethod(<<"PUT">>, _Body, Req) -> 
    cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, parse_request(Req), Req).


%%% 
parse_request(Request) ->
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
            ErrorResponse = #{
                date_time => utils:current_time_string(),
                error_code => 404,
                error_text => <<"Date range not found">>
            },
            jiffy:encode(ErrorResponse);
        _ ->

            JsonFreindly = lists:map(fun(DbItem) ->
                                #{url => DbItem#apodimagetable.url,
                                             copyright => DbItem#apodimagetable.copyright,
                                             date => list_to_binary(gregorian_days_to_string_date(DbItem#apodimagetable.date)),
                                             explanation => DbItem#apodimagetable.explanation,
                                             hdurl => DbItem#apodimagetable.hdurl,
                                             media_type => DbItem#apodimagetable.media_type,
                                             service_version => DbItem#apodimagetable.service_version,
                                             title => DbItem#apodimagetable.title}                                 
                                         end, ListOfRecords),
            io:format("Reqeust: ~p -- ~p~n", [gregorian_days_to_string_date(StartDate), gregorian_days_to_string_date(EndDate)]),
            io:format("Rerturn: ~p records~n", [length(JsonFreindly)]),
            jiffy:encode(JsonFreindly)
    end.
