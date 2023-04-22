% @Author: oleg
% @Date:   2022-09-27 14:59:44
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-31 18:21:24

-module(db_access).

-import(utils, [date_to_gregorian_days/1, gregorian_days_to_binary/1]).
-include_lib("stdlib/include/ms_transform.hrl").
-include("include/apodtelemetry.hrl").
-include("include/youtube_channel.hrl").
-include("include/apod_record_def.hrl").
-include("include/macro_definitions.hrl").
-include("include/server_config_item.hrl").
-include("include/client_profile_table.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

 -export([process_date_request/2, 
           fetch_channel_videos/1, 
           update_nasa_table/1, 
           package_channel_data/1, 
           add_channel_request/2, 
           fetch_videos_for_channel_id/1, 
           purge_table/1, 
           get_channel_descriptors_for_client/1,
           get_client_youtube_key/1]).

 -export([dump_db/0, get_all_keys/1, count_media_type/1, dump_telemetry_table/0, get_dataset_size/2]).
-compile(export_all).

update_nasa_table(DBItem) -> 
    Fun = 
        fun() ->
            mnesia:write(DBItem)            %% if this is an image, store it
        end,
    mnesia:transaction(Fun). %% execute the transaction


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
            case apod_data_aquisition:fetch_apod_data(notfound, StartDate, EndDate) of
                {error, _} ->
                    io:format("NASA fetch failed~n"),
                    date_range_not_found(StartDate, EndDate);
                {ok, JsonResult} ->
                    Start = calendar:gregorian_days_to_date(StartDate),
                    End = calendar:gregorian_days_to_date(EndDate),
                    io:format("fetching date range from NASA: ~p -- ~p~n",[Start, End]),
                    utils:update_database(apod, JsonResult),
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


add_channel_request(ClientKey, Name) ->
    ChannelName = list_to_binary(string:replace(string:lowercase(Name), " ", "", all)), %%% Normalize
    {_, ListOfRecords} = fetch_client_data(client_profile_table_pending, ClientKey), %%% Get the record if it exists
    case length(ListOfRecords) of %%% should be a list of one item
        0 ->
            %%% this client does not have this youtube channel in their profile. Add it.
            add_channel_name_to_pending_channel_table(emptylist, ClientKey, ListOfRecords, ChannelName),
            ok;
        _ ->
            %%% this client already has this youtube channel in their profile. Let them know.
            add_channel_name_to_pending_channel_table(nonemptylist, ClientKey, ListOfRecords, ChannelName)
    end.

add_channel_name_to_pending_channel_table(emptylist, ClientID, [], ChannelName) ->
    UpdatedRecord = #client_profile_table_pending{
        client_id = ClientID,
        channel_list = [ChannelName]    
    },

    Fun = 
        fun() ->
            mnesia:write(UpdatedRecord)
        end,
    mnesia:transaction(Fun); %% execute the transaction

add_channel_name_to_pending_channel_table(nonemptylist, _ClientKey, [ClientRecord], ChannelName) ->
    Found = lists:member(ChannelName, ClientRecord#client_profile_table_pending.channel_list),
    case Found  of
        false ->
            NewList = lists:append(ClientRecord#client_profile_table_pending.channel_list, [ChannelName]),
            UpdatedRecord = #client_profile_table_pending{
                client_id = ClientRecord#client_profile_table_pending.client_id,
                channel_list = NewList    
            },
            Fun = 
                fun() ->
                    mnesia:write(UpdatedRecord)
                end,
            mnesia:transaction(Fun),
            {ok, #{channel_added => ChannelName }};
        true ->
            {ok, #{channel_exists => ChannelName }}

    end.


fetch_videos_for_channel_id(ChannelID) ->
    Match = ets:fun2ms(
        fun(Record) 
            when Record#youtube_channel.channel_id =:= ChannelID ->
                Record
        end),
    SelectRecords = fun() -> mnesia:select(youtube_channel, Match) end,
    mnesia:sync_transaction(SelectRecords).

fetch_client_data(client_profile_table, ClientID) ->
    Match = ets:fun2ms(
        fun(Record) 
            when Record#client_profile_table.client_id =:= ClientID ->
                Record
        end),
    SelectRecords = fun() -> mnesia:select(client_profile_table, Match) end,
    mnesia:sync_transaction(SelectRecords);

fetch_client_data(client_profile_table_pending, ClientID) ->
    Match = ets:fun2ms(
        fun(Record) 
            when Record#client_profile_table_pending.client_id =:= ClientID ->
                Record
        end),
    SelectRecords = fun() -> mnesia:select(client_profile_table_pending, Match) end,
    mnesia:sync_transaction(SelectRecords).

fetch_channel_videos(ClientKey) ->
    {Code, Map} = server_config_processor:fetch_client_config_data_db(json, ClientKey),
    format_channel_videos(ClientKey, {Code, Map}).

format_channel_videos(ClientKey, {error, _Map}) ->
    Message = <<"client id: ", ClientKey/binary, "not found">>,
    {error, jiffy:encode(#{error => utils:compose_error_message(500, Message)})};

format_channel_videos(_ClientKey, {ok, Map}) -> 
    [ListOfMaps] = maps:values(Map),
    ListOfChannelIDS = format_channel_videos_helper(ListOfMaps, []),
    collect_all_videos(ListOfChannelIDS, []).

collect_all_videos([ChannelID|Tail], Acc) ->
    {_, ListOfRecords} = fetch_channel_data_from_db(ChannelID),
    collect_all_videos(Tail, lists:append(Acc, ListOfRecords)).


format_channel_videos_helper([], Acc) -> Acc;
format_channel_videos_helper([ChannelDescriptor|Tail], Acc) ->
    ChannelID = maps:get(channel_id, ChannelDescriptor),
    NewList = lists:append(Acc, [ChannelID]),
    format_channel_videos_helper(Tail, NewList).

date_range_not_found(StartDate, EndDate) ->
    DateStart = gregorian_days_to_binary(StartDate),
    DateEnd = gregorian_days_to_binary(EndDate),
    Message = <<"Date range not found: ">>,
    ErrorResponse = #{
        <<"date_time">> => utils:current_time_string(),
        <<"error_code">> => 404,
        <<"error_text">> => <<Message/binary, DateStart/binary, <<" -- ">>/binary, DateEnd/binary>>
    },
    {not_found, jiffy:encode(ErrorResponse)}.   

fetch_channel_data_from_db(Channel) ->
    Match = ets:fun2ms(
        fun(Record) 
            when Record#youtube_channel.channel_id =:= Channel ->
                Record
        end),
    SelectRecords = fun() -> mnesia:select(youtube_channel, Match) end,
    mnesia:sync_transaction(SelectRecords).

package_channel_data(ListOfRecords) ->
    Predicate = fun(Lhs, Rhs) ->
        if
            Rhs#youtube_channel.date =< Lhs#youtube_channel.date ->
                true;
            true -> false
        end
    end,
    SortedList = lists:sort(Predicate, ListOfRecords),
    MapData = lists:map(fun(DbItem) ->
        #{channel_id    => DbItem#youtube_channel.channel_id,
            video_id      => DbItem#youtube_channel.video_id,
            url_medium    => DbItem#youtube_channel.url_medium,
            width         => DbItem#youtube_channel.width,
            height        => DbItem#youtube_channel.height,
            title         => DbItem#youtube_channel.title,
            date          => DbItem#youtube_channel.date
        }                                 
        end, SortedList),
    maps:put(?TOP_KEY, MapData, #{}).
    

%%% Given a table name, delete every record
purge_table(TableName) ->
try 
  Keys = mnesia:activity(transaction, 
    fun() -> 
      mnesia:all_keys(TableName) 
    end), 

  lists:foreach( 
    fun(Key) -> 
      mnesia:activity(transaction, 
        fun() -> 
          mnesia:delete({TableName, Key}) 
          end) 
    end, Keys)
catch
    _Class:{aborted,{Reason,{TableName,_}}} ->
        io:format("Failed to purge table name: ~p with reason: ~p~n", [TableName, Reason]);

    Class:Exception ->
        io:format("Class: ~p~nException: ~p~n", [Class, Exception])
end.

get_channel_descriptors_for_client(ClientID) ->
    ReaderFun = fun() -> mnesia:read(client_profile_table, ClientID) end, 
    case mnesia:transaction(ReaderFun) of
        {aborted,{Reson,_}} -> 
            {error, Reson};
        {atomic, []} ->
            {error, no_records};
        {_, [Record]} ->
            {ok, Record#client_profile_table.channel_list}
    end.

get_client_youtube_key(ClientID) ->
    ReaderFun = fun() -> mnesia:read(client_profile_table, ClientID) end, 
    case mnesia:transaction(ReaderFun) of
        {aborted,{Reson,_}} -> 
            {error, Reson};
        {atomic, []} ->
            {error, no_records};
        {_, [Record]} ->
            {ok, Record#client_profile_table.youtube_key}
    end.

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
    {_, ListOfRecords} = mnesia:sync_transaction(SelectRecords),
    length(ListOfRecords).

get_all_keys(TableName) ->
    Transaction = fun() ->
        mnesia:all_keys(TableName)
    end, 
    {atomic, KeyList} = mnesia:sync_transaction(Transaction),
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


