-module(star_watch_handler).
-behavior(cowboy_handler).

-export([init/2]).
-include("include/apod_record_def.hrl").

init(Req0, State) ->
    Parsed = parse_request(Req0),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Parsed,
        Req0),
    {ok, Req, State}.

parse_request(Request) ->
    #{
        start_date  := StartDate,
        end_date    := EndDate
    } = cowboy_req:match_qs([start_date, end_date], Request),
    process_date_request(StartDate, EndDate, Request).

process_date_request(StartDate, EndDate, Req) ->
    [R] = mnesia:dirty_index_read(apodimagetable, StartDate, date),
    [E] = mnesia:dirty_index_read(apodimagetable, EndDate, date),
    Term = #{url => R#apodimagetable.url,
             copyright => R#apodimagetable.copyright,
             date => R#apodimagetable.date,
             explanation => R#apodimagetable.explanation,
             hdurl => R#apodimagetable.hdurl,
             media_type => R#apodimagetable.media_type,
             service_version => R#apodimagetable.service_version,
             title => R#apodimagetable.title},
    Json = jiffy:encode(Term),
    {IP, Port} = cowboy_req:peer(Req),
    {A, B, C, D} = IP,
    Ip = io_lib:format("~p.~p.~p.~p", [A, B, C, D]),
    TestTerm = #{
        ip => list_to_binary(Ip),
        port => Port
    },
    TestEncode = jiffy:encode(TestTerm),
    TestEncode.

