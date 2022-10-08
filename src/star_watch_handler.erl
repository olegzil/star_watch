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
%%% Add error handling to the return value of cowboy_req(). 
%%% If start_date or end_date are empty the function will return {badkey,start_date} or {badkey,end_date}
%%% A json encoded error message should be returned to the caller.
parse_request(Request) ->
    #{
        start_date  := StartDate,
        end_date    := EndDate %{Name, Constraints, Default}
    } = cowboy_req:match_qs([{start_date, nonempty}, {end_date, nonempty}], Request),
    process_date_request(date_to_gregorian_days(StartDate), date_to_gregorian_days(EndDate), Request).


process_date_request(StartDate, EndDate, Req) ->
    GetStartDate = fun() -> mnesia:index_read(apodimagetable, StartDate, date) end,
    GetEndDate   = fun() -> mnesia:dirty_index_read(apodimagetable, EndDate, date) end,
    {atomic, [R]} = mnesia:transaction(GetStartDate),
    {atomic, [E]} = mnesia:transaction(GetEndDate),

    Term = #{url => R#apodimagetable.url,
             copyright => R#apodimagetable.copyright,
             date => list_to_binary(gregorian_days_to_string_date(R#apodimagetable.date)),
             explanation => R#apodimagetable.explanation,
             hdurl => R#apodimagetable.hdurl,
             media_type => R#apodimagetable.media_type,
             service_version => R#apodimagetable.service_version,
             title => R#apodimagetable.title},
    {IP, Port} = cowboy_req:peer(Req),
    {A, B, C, D} = IP,
    Ip = io_lib:format("~p.~p.~p.~p", [A, B, C, D]),
    TestTerm = #{
        ip => list_to_binary(Ip),
        port => Port
    },
    TestEncode = jiffy:encode([TestTerm, Term]).

