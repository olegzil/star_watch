% @Author: oleg
% @Date:   2022-09-27 14:59:44
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-11-18 19:35:42

-module(db_access).

-import(utils, [date_to_gregorian_days/1, gregorian_days_to_binary/1]).
-include_lib("stdlib/include/ms_transform.hrl").
-include("include/apodtelemetry.hrl").
-include("include/apod_record_def.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([process_date_request/2]).
-export([dump_db/0, get_all_keys/1, count_media_type/1, dump_telemetry_table/0, get_dataset_size/2]).

get_dataset_size(StartDate, EndDate) ->
    Match = ets:fun2ms(
        fun(Record) 
            when Record#apodimagetable.date >= StartDate, 
                 Record#apodimagetable.date =< EndDate ->
                Record
        end),

    SelectRecords = fun() -> mnesia:select(apodimagetable, Match) end,
    {_, ListOfRecords} = mnesia:transaction(SelectRecords),
    {
        ok, jiffy:encode(
                #{size => length(ListOfRecords)}
            )
    }.

process_date_request(StartDate, EndDate) ->
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
            case utils:fetch_apod_data(StartDate, EndDate, notfound) of
                {error, _} ->
                    io:format("NASA fetch failed~n"),
                    date_rage_not_found(StartDate, EndDate);
                {ok, JsonResult} ->
                    Start = calendar:gregorian_days_to_date(StartDate),
                    End = calendar:gregorian_days_to_date(EndDate),
                    io:format("fetching date range from NASA: ~p -- ~p~n",[Start, End]),
                    utils:update_database(JsonResult),
                    {ok, JsonResult}
            end;

        _ ->
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Debug functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dump_telemetry_table() ->
    Fun = fun(#apodtelemetry{
        uuid = Uuid, 
        access_tally = Tally,
        statsshowfavorites = StatsFavorites,
        statshelpmain = StatsHelpMain,
        statshelpfavorites = StatsHelpFavorites,
        statshelpdetail = StatsHelpDetail,
        statssettings = StatsSettings,
        statssearch = StatsSearch,
        statsdoubletapselect = StatsDoubleTapSelect,
        statslongpresstoselect = StatsLongPressSelect,
        statszoompan = StatsZoomPan,
        statssetwallpaper = StatsWallpapers
        }, Acc) ->
        lists:append(Acc, [
            list_to_binary(io_lib:format("~p~p", ["Uuid:", Uuid])),
            list_to_binary(io_lib:format("~p~p", ["Tally:",Tally])),
            list_to_binary(io_lib:format("~p~p", ["StatsFavorites:",StatsFavorites])),
            list_to_binary(io_lib:format("~p~p", ["StatsHelpMain",StatsHelpMain])),
            list_to_binary(io_lib:format("~p~p", ["StatsHelpFavorites:",StatsHelpFavorites])),
            list_to_binary(io_lib:format("~p~p", ["StatsHelpDetail:",StatsHelpDetail])),
            list_to_binary(io_lib:format("~p~p", ["StatsDoubleTapSelect:",StatsDoubleTapSelect])),
            list_to_binary(io_lib:format("~p~p", ["StatsLongPressSelect:",StatsLongPressSelect])),
            list_to_binary(io_lib:format("~p~p", ["StatsZoomPan:",StatsZoomPan])),
            list_to_binary(io_lib:format("~p~p", ["StatsWallpapers:",StatsWallpapers])),
            list_to_binary(io_lib:format("~p~p", ["StatsSettings:",StatsSettings])),
            list_to_binary(io_lib:format("~p~p", ["StatsSearch:",StatsSearch]))
            ])
    end, 
    Transaction = fun() ->
      mnesia:foldr(Fun, [], apodtelemetry)
    end,
    {atomic, Result} = mnesia:transaction(Transaction),
    Result.

count_media_type(MediaType) ->
    SearchTerm = atom_to_binary(MediaType),
    io:format("Search term = ~p~n", [SearchTerm]),
    Match = ets:fun2ms(
        fun(Record) 
            when Record#apodimagetable.media_type =:= SearchTerm ->
                Record
        end),

    SelectRecords = fun() -> mnesia:select(apodimagetable, Match) end,
    {_, ListOfRecords} = mnesia:transaction(SelectRecords),
    length(ListOfRecords).

get_all_keys(TableName) ->
    Transaction = fun() ->
        mnesia:all_keys(TableName)
    end, 
    {atomic, KeyList} = mnesia:transaction(Transaction),
    {ok, File} = file:open("/home/oleg/temp/key_list.txt", [write]),
    io:format(File, "~p~n", [KeyList]).

dump_db() ->
	Fun = fun(#apodimagetable{date = Date}, Acc) ->
		lists:append(Acc, [utils:gregorian_days_to_binary(Date)])
	end, 
	Transaction = fun() ->
	  mnesia:foldr(Fun, [], apodimagetable)
	end,
	{atomic, DateList} = mnesia:transaction(Transaction),
	{ok, File} = file:open("/home/oleg/temp/dates.txt", [write]),
	io:format(File, "~p~n", [DateList]).


