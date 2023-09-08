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
           fetch_videos_for_channel_id/1, 
           fetch_client_data/2,
           purge_table/1, 
           get_channel_descriptors_for_client/1,
           get_client_youtube_key/1,
           get_channel_data/2,
           does_record_exist/2,
           delete_video_link_from_profile_table/2,
           delete_channel_from_client/2,
           get_valid_client_id/1,
           is_video_in_youtube_channel/2,
           fetch_video_data/2,
           construct_key/2,
           get_video_record/1,
           is_channel_in_db/1]).

-export([dump_db/0, get_all_keys/1, count_media_type/1, dump_telemetry_table/0, get_dataset_size/2]).

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

fetch_videos_for_channel_id(ChannelID) ->
    ReaderFun = fun() -> mnesia:index_read(youtube_channel, ChannelID, #youtube_channel.channel_id) end,
    {atomic, Records} = mnesia:transaction(ReaderFun),
    Predicate = fun(Lhs, Rhs) ->
        if
            Rhs#youtube_channel.date =< Lhs#youtube_channel.date ->
                true;
            true -> false
        end
    end,
    lists:sort(Predicate, Records).

fetch_client_data(client_profile_table, ClientID) ->
    Match = ets:fun2ms(
        fun(Record) 
            when Record#client_profile_table.client_id =:= ClientID ->
                Record
        end),
    SelectRecords = fun() -> mnesia:select(client_profile_table, Match) end,
    mnesia:sync_transaction(SelectRecords).

fetch_channel_videos(ClientKey) ->
    {Code, Map} = server_config_processor:fetch_client_config_data_db(json, ClientKey),
    format_channel_videos(ClientKey, {Code, Map}).

format_channel_videos(ClientKey, {error, _Map}) ->
    Message = <<"client id: ", ClientKey/binary, "not found">>,
    {error, jiffy:encode(#{error => utils:compose_error_message(500, Message)})};

format_channel_videos(_ClientKey, {ok, Map}) -> 
    List = maps:get(channel_list,Map),
    ListOfChannelIDS = format_channel_videos_helper(List, []),
    ChannelVideos = collect_all_videos(ListOfChannelIDS, []),
    youtube_channel_record_to_map(ChannelVideos, []).

youtube_channel_record_to_map([], Acc) ->
    Acc;
youtube_channel_record_to_map([ChannelVideos | Tail], Acc) ->
    NewAcc = #{
        video_id => ChannelVideos#youtube_channel.video_id,
        channel_id => ChannelVideos#youtube_channel.channel_id,
        url_medium => ChannelVideos#youtube_channel.url_medium,
        width => ChannelVideos#youtube_channel.width,
        height => ChannelVideos#youtube_channel.height,
        title => ChannelVideos#youtube_channel.title,
        date => ChannelVideos#youtube_channel.date
    },
    youtube_channel_record_to_map(Tail, lists:append(Acc, [NewAcc])).

collect_all_videos([], Acc) ->
    Acc;
collect_all_videos([ChannelID|Tail], Acc) ->
    {_, ListOfRecords} = fetch_channel_data_from_db(ChannelID),
    collect_all_videos(Tail, lists:append(Acc, ListOfRecords)).


format_channel_videos_helper([], Acc) -> Acc;
format_channel_videos_helper([{_ChannelName, ChannelID} | Tail], Acc) ->
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
    MapData = lists:map(fun(DbItem) ->
        #{channel_id    => DbItem#youtube_channel.channel_id,
            video_id      => DbItem#youtube_channel.video_id,
            url_medium    => DbItem#youtube_channel.url_medium,
            width         => DbItem#youtube_channel.width,
            height        => DbItem#youtube_channel.height,
            title         => DbItem#youtube_channel.title,
            date          => DbItem#youtube_channel.date
        }                                 
        end, ListOfRecords),
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
        {aborted,{Reason,_}} -> 
            {error, Reason};
        {atomic, []} ->
            {error, no_records};
        {_, [Record]} ->
            {ok, Record#client_profile_table.channel_list}
    end.

get_client_youtube_key(ClientID) ->
    ReaderFun = fun() -> mnesia:read(client_profile_table, ClientID) end, 
    case mnesia:transaction(ReaderFun) of
        {aborted,{Reason,_}} -> 
            {error, Reason};
        {atomic, []} ->
            {error, no_records};
        {_, [Record]} ->
            {ok, Record#client_profile_table.youtube_key}
    end.

get_video_record(Key) ->
    ReaderFun = fun() -> mnesia:read(youtube_channel, Key) end,
    case mnesia:transaction(ReaderFun) of
        {atomic, []} ->
            error;
        {atomic, [Record]} ->
            {ok, Record}
    end.

is_channel_in_db(ChannelID) ->
    ReaderFun = fun() -> mnesia:index_read(youtube_channel, ChannelID, #youtube_channel.channel_id) end,
    {atomic, Records} = mnesia:transaction(ReaderFun),
    case length(Records) of
        0 ->
            false;
        _ ->
            true
    end.

%%%
%%% Determine if the client has this video link. First, use and indexed read to determine if the link exits at all.
%%% If it does not, report false.
%%% If it does, read each record and extract the channel id from its key. Compare the channel id to the value in 
%%% the config record of the client. If they match return true, false otherwise.
%%%
is_video_in_youtube_channel(ClientID, Link) ->
    Parts = string:split(Link, "/", all),
    VideoID = lists:nth(4, Parts),
    ReaderFun = fun() -> mnesia:index_read(youtube_channel, VideoID, #youtube_channel.video_id) end,
    {atomic, Records} = mnesia:transaction(ReaderFun),
    case length(Records) of
        0 ->
            false;
        _->
            [Record] = Records,
            [ChannelID, _VideoID] = string:split(Record#youtube_channel.key, ":", all),
            {ok, ClientRecord} = server_config_processor:fetch_client_config_data_db(json, ClientID),
            ChannelList = maps:get(channel_list, ClientRecord),
            Found = lists:keyfind(ChannelID, 2, ChannelList),
            if
                Found =:= false ->
                    false;
                true -> 
                    true
            end
    end.

fetch_video_data(ClientID, ChannelID) ->
    Records = get_channel_data_db(ChannelID), %% get all videos for this channel
    SortByDatePredicate = fun(Lhs, Rhs) ->
        if
            Rhs#youtube_channel.date =< Lhs#youtube_channel.date ->
                true;
            true ->
                false
        end
    end,

    SortedList = lists:sort(SortByDatePredicate, Records), %% Sort the videos by date
    [VideoDescriptor | _] = SortedList, %% just the latest video 
    {ok, ClientProfileRecord} = server_config_processor:fetch_config_data_for_client(ClientID),
    ClientChannels = ClientProfileRecord#client_profile_table.channel_list,
    case lists:keyfind(ChannelID, 2, ClientChannels) of
        false ->
            {false, no_such_channel};
        {Name, _} ->
            {ok, #{
                video_id => VideoDescriptor#youtube_channel.video_id,
                channel_id => VideoDescriptor#youtube_channel.channel_id,
                url_medium => VideoDescriptor#youtube_channel.url_medium,
                width => VideoDescriptor#youtube_channel.width,
                height => VideoDescriptor#youtube_channel.height,
                title => VideoDescriptor#youtube_channel.title,
                date => VideoDescriptor#youtube_channel.date,
                author => Name
            }}
    end.

%%%
%%% Get the latest image associate with this channel
%%%
get_channel_data(ClientID, ChannelID) ->
    Records = get_channel_data_db(ChannelID),
    Predicate = fun(Lhs, Rhs) ->
        if
            Rhs#youtube_channel.date =< Lhs#youtube_channel.date ->
                true;
            true -> false
        end
    end,
    case length(Records) of
        0 ->
            UpdatedRecord = process_channel_list(ClientID, ChannelID, get_channel_data_db(ChannelID)),
            SortedList = lists:sort(Predicate, UpdatedRecord),
            [First | _] = SortedList,
            First;
        _->
            SortedList = lists:sort(Predicate, Records),
            [First | _] = SortedList,
            First
    end.

get_channel_data_db(ChannelID) ->
    ReaderFun = fun() -> mnesia:index_read(youtube_channel, ChannelID, #youtube_channel.channel_id) end,
    {atomic, Records} = mnesia:transaction(ReaderFun),
    Records.

does_record_exist(TargetID, TableName) ->
    Result = mnesia:transaction(fun() -> mnesia:read(TableName, TargetID)  end),
    case Result of 
        {atomic, []} -> false;
        {atomic, _} -> true
    end.

process_channel_list(ClientID, ChannelID, []) ->
    db_access_server:fetch_channel_data(serverfirst, ClientID, ChannelID),
    get_channel_data_db(ChannelID);
process_channel_list(_ClientID, _ChannelID, NoneEmptyList) -> 
    NoneEmptyList.

%%% This function deletes a channel associated with this client id. If the cient ID is not found, 
%%% the funnction assumes it's a new ID. In this case, the channels from the default client id
%%% are serched for the target clientID. If not found, an error is returned. If found, all channels
%%% from the default client id are coppied to the new client id and the target id is deleted.
delete_channel_from_client(ClientID, ChannelID) ->
    ReadResult = mnesia:transaction(fun() -> mnesia:read(client_profile_table, ClientID) end),
    case ReadResult of
        {atomic, []} ->
            use_default_client(ClientID, ChannelID);
        {atomic, [Record]} ->
            NewList = lists:keydelete(ChannelID, 2, Record#client_profile_table.channel_list),
            L1 = length(NewList),
            L2 = length(Record#client_profile_table.channel_list),
            if
                L1 =:= L2 ->
                    utils:format_error(?SERVER_ERROR_NO_SUCH_CHANNEL, <<"channel id: ", ChannelID/binary, " not found">>);
                NewList =/= [] ->
                    UpdatedRecord = Record#client_profile_table{
                        channel_list = NewList
                    },
                    mnesia:transaction(fun() -> mnesia:write(UpdatedRecord) end),
                    utils:package_channel_record_list(fetch_videos_for_channel_id(ChannelID));
                true ->
                    utils:format_error(?SERVER_ERROR_NO_SUCH_CLIENT, <<"client id: ", ClientID/binary, " does not exist">>)
            end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Given the default record that contains multiple channels and a channel id     %%%
%%% this function returns a tuple: {<<"Title">>, <<"Channel ID">>} for an exiting %%% 
%%% channel id in the default client record or false if the channel id does not   %%%
%%% exist in the default client record.                                           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extract_client_channel_or_error(DefaultRecord, ChannelID) ->
    case DefaultRecord of
        {error, no_such_client} ->
            {error, no_such_client};
        {ok, Record} ->
            SearchResult = lists:keyfind(ChannelID, 2, Record#client_profile_table.channel_list),
            {SearchResult, Record}
    end.

use_default_client(ClientID, ChannelID) ->
    DefaultClient = server_config_processor:get_client_key(?SERVER_CONFIG_FILE),
    DefaultRecord = server_config_processor:fetch_client_config_data_db(not_json, DefaultClient),
    case extract_client_channel_or_error(DefaultRecord, ChannelID) of
        {error, no_such_client} ->
            utils:format_error(?SERVER_ERROR_NO_SUCH_CLIENT, no_such_client);
        {false, _} ->
            utils:format_error(?SERVER_ERROR_NO_SUCH_CHANNEL, no_such_channel);
        {_, Record} ->
            mnesia:transaction(fun()-> 
                NewList = lists:keydelete(ChannelID, 2, Record#client_profile_table.channel_list),
                mnesia:write(Record#client_profile_table{
                    client_id = ClientID,
                    channel_list = NewList
                })
            end),
            {ok, Message} = utils:format_success(?SERVER_ERROR_OK, <<"channelID: ", ChannelID/binary, " has been deleted">>),
            {ok, jiffy:encode(Message)}
    end.

get_valid_client_id(ClientID) ->
    Present = server_config_processor:is_client_in_profile_map(ClientID),
    if
        Present =:= true ->
            ClientID;
        true ->
            server_config_processor:get_client_key(?SERVER_CONFIG_FILE)
    end.

delete_video_link_from_profile_table(ClientID, VideoLink) ->
    {atomic, Result} = mnesia:transaction(fun()-> mnesia:read(client_profile_table, ClientID) end),
    case Result of
        [] ->
            {error, VideoLink};
        _ ->
            mnesia:transaction(fun()-> mnesia:delete({client_profile_table, ClientID}) end),
            {ok, ClientID}
    end.
construct_key(ChannelID, VideoID) ->
    <<ChannelID/binary, ":", VideoID/binary>>.

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
