% @Author: Oleg Zilberman
% @Date:   2022-10-08 13:34:16
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-31 16:58:16
-module(utils).
-export([date_to_gregorian_days/1, 
		 gregorian_days_to_binary/1, 
		 current_time_string/0,
		 start_cron_job/1,
		 time_pair_to_fetch/2,
		 update_client_record/1, 
		 process_file_list/2,
		 update_db_from_json_file/1,
		 update_database/2,
		 find_token_in_string/2,
		 generate_coparable_list/1,
		 write_test_data_to_db_and_dump_to_file/0,
		 test_multi_channel_data_fetch/0,
		 reformat_channel_data/1,
		 compose_error_message/2,
		 jsonify_list_of_tuples/2, 
		 jsonify_client_profile_table/1,
		 format_error/2,
		 package_channel_record_list/1]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("include/apodtelemetry.hrl").
-include("include/apod_record_def.hrl").
-include("include/youtube_channel.hrl").
-include("include/macro_definitions.hrl").
-include("include/client_profile_table.hrl").
-include("src/include/registration_query.hrl").

date_to_gregorian_days(<<A,B,C,D,E,F,G,H,I,J>>) ->
	Date = <<A,B,C,D,E,F,G,H,I,J>>,
    DateTuple = list_to_tuple(lists:map(fun(Item)-> binary_to_integer(Item) end, string:split(Date, "-", all))),
    calendar:date_to_gregorian_days(DateTuple);

date_to_gregorian_days(<<A,B,C,D>>) ->
	Date = <<A,B,C,D>>,
    Tuple = list_to_tuple(lists:map(fun(Item)-> binary_to_integer(Item) end, string:split(Date, "", all))),
    DateTuple = erlang:insert_element(2,erlang:insert_element(2, Tuple, 1), 1),
    calendar:date_to_gregorian_days(DateTuple);

date_to_gregorian_days(Date) ->
	[YearB, MonthB, DateTimeB] = string:split(Date,"-", all),
	Year = binary_to_integer(YearB),
	Month = binary_to_integer(MonthB),
	DateList = string:split(DateTimeB, "T"),
	[DatePart|_] = DateList,
	DateValue = binary_to_integer(DatePart),
	calendar:date_to_gregorian_days({Year, Month, DateValue}).

gregorian_days_to_binary(Date) ->
	GregorianDate = calendar:gregorian_days_to_date(Date),
	List = tuple_to_list(GregorianDate), 
	ListOfStrings = lists:map(fun(Item)-> integer_to_list(Item) end, List),
	list_to_binary(string:join(ListOfStrings, "-")).

update_database(apod, Data) ->
    try jiffy:decode(Data, []) of
    	Map ->
		    %% insert the json data into mnesia
		    insert_db_entries(apod, Map)
    catch
    	Class:Reason ->
		    io:format("Filed insert~n"),
    		io:format("~p~n ~p~n", [Class, Reason])
    end;

update_database(youtube, Data) ->
    try jiffy:decode(Data, [return_maps]) of
    	Map ->
		    %% insert the json data into mnesia
    		insert_db_entries(youtube, Map),
		    {ok, Map}

    catch
    	Class:Reason ->
    		Message = io_lib:format("~p~n ~p~n", [Class, Reason]),
    		{error, Message}
    end.

current_time_string() ->
	{{Year, Month, Day}, {Hour, Min, Sec}} = 
	calendar:now_to_datetime(erlang:timestamp()),
	list_to_binary(io_lib:format("~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.0+00:00", 
		[Year, Month, Day, Hour, Min, Sec])).


time_pair_to_fetch(now, _) ->
	{{Year, Month, Day}, {_, _, _}} = 
	calendar:now_to_datetime(erlang:timestamp()),
	binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p-~.2.0p-~.2.0p",[Year, Month, Day])));

time_pair_to_fetch(future, TimeDeltaInSeconds) ->
	Now = calendar:now_to_datetime(erlang:timestamp()),
	Seconds = calendar:datetime_to_gregorian_seconds(Now),
	FutureSeconds = Seconds + TimeDeltaInSeconds,
	{{Year, Month, Day}, {_, _, _}} = 
		calendar:gregorian_seconds_to_datetime(FutureSeconds),
	binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p-~.2.0p-~.2.0p",[Year, Month, Day])));

time_pair_to_fetch(past, TimeDeltaInSeconds) ->
	Now = calendar:now_to_datetime(erlang:timestamp()),
	Seconds = calendar:datetime_to_gregorian_seconds(Now),
	FutureSeconds = Seconds - TimeDeltaInSeconds,
	{{Year, Month, Day}, {_, _, _}} = 
		calendar:gregorian_seconds_to_datetime(FutureSeconds),
	binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p-~.2.0p-~.2.0p",[Year, Month, Day]))).

start_cron_job(apod) ->
	ImageOfTheDayJob = {{daily, {12, 30, am}},
    {apod_data_aquisition, fetch_data, [periodic]}},
    erlcron:cron(apod_daily_fetch_job, ImageOfTheDayJob);

start_cron_job(youtube) -> 
	{{Year, Month, Day},{_, _, _}} = calendar:universal_time(), % get current year/month/day
	GregorianDate = calendar:date_to_gregorian_days({Year, Month, Day}), % convert it to a single number
	{Y, M, D} = calendar:gregorian_days_to_date(GregorianDate),	% back track the current date back one day
	Date = list_to_binary(io_lib:format("~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0wZ", [Y, M, D, 0, 0, 0])), % generate 
	FetchResult = server_config_processor:fetch_list_of_channel_ids_and_youtube_keys_db(),
	[ClientProfiles] = maps:values(FetchResult),
	YoutubeChannelFetchJob = {{daily, {12, 30, am}},
    {youtube_data_aquisition, fetch_data, [periodic, ClientProfiles, [Date]]}},
    erlcron:cron(youtube_daily_fetch_job, YoutubeChannelFetchJob).

update_client_record(Telemetry) ->
	{Uuid, Atom} = Telemetry,
	Action = list_to_atom(
				string:lowercase(
					binary_to_list(
						atom_to_binary(Atom)
					)
				)
			), %make sure atom is lowercase
	case {Uuid, Action} of 
		{Uuid, isregistered} -> 
			Match = ets:fun2ms(
			    fun(Record) 
			        when Record#apodtelemetry.uuid =:= Uuid->
			            Record
			    end),
			case mnesia:transaction(fun() -> mnesia:select(apodtelemetry, Match) end) of
				{atomic, [_Record]} -> 
		    				ReturnValue = #{
		    					uuid => Uuid,
		    					registered => true
		    				},
		    				ReturnValue;
			   	{atomic, []} -> 
		    				ReturnValue = #{
		    					uuid => Uuid,
		    					registered => false
		    				},
		    				io:format("record NOT found: ~p~n", [Uuid]),
		    				ReturnValue
			end;

		{Uuid, statsregisterapp} ->
			UpdateFun = fun() -> 
				mnesia:write(#apodtelemetry{uuid = Uuid})
			end,
			mnesia:transaction(UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{Uuid, statsupdateaccess} ->
			UpdateFun =	fun(Record) -> 
				Tally = Record#apodtelemetry.access_tally + 1,
				mnesia:write(Record#apodtelemetry{access_tally = Tally + 1})
			end,
			find_client_uuid_and_update(Uuid, UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{Uuid, statsshowfavorites} ->
			UpdateFun =	fun(Record) -> 
				Tally = Record#apodtelemetry.statsshowfavorites + 1,
				mnesia:write(Record#apodtelemetry{statsshowfavorites = Tally})
			end,
			find_client_uuid_and_update(Uuid, UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{Uuid, statshelpmain} ->
			UpdateFun =	fun(Record) -> 
				Tally = Record#apodtelemetry.statshelpmain + 1,
				mnesia:write(Record#apodtelemetry{statshelpmain = Tally})
				end,
			find_client_uuid_and_update(Uuid, UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{Uuid, statshelpdetail} ->
			UpdateFun =	fun(Record) -> 
				Tally = Record#apodtelemetry.statshelpdetail + 1,
				mnesia:write(Record#apodtelemetry{statshelpdetail = Tally})
			end,
			find_client_uuid_and_update(Uuid, UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{Uuid, statshelpfavorites} ->
			UpdateFun =	fun(Record) -> 
				Tally = Record#apodtelemetry.statshelpfavorites + 1,
				mnesia:write(Record#apodtelemetry{statshelpfavorites = Tally})
			end,
			find_client_uuid_and_update(Uuid, UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{Uuid, statshelpdaterange} ->
			UpdateFun =	fun(Record) -> 
				Tally = Record#apodtelemetry.statshelpdaterange + 1,
				mnesia:write(Record#apodtelemetry{statshelpdaterange = Tally})
			end,
			find_client_uuid_and_update(Uuid, UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{Uuid, statssettings} ->
			UpdateFun =	fun(Record) -> 
				Tally = Record#apodtelemetry.statssettings + 1,
				mnesia:write(Record#apodtelemetry{statssettings = Tally})
			end,
			find_client_uuid_and_update(Uuid, UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{Uuid, statssearch} ->
			UpdateFun =	fun(Record) -> 
				Tally = Record#apodtelemetry.statssearch + 1,			
				mnesia:write(Record#apodtelemetry{statssearch = Tally})
			end,
			find_client_uuid_and_update(Uuid, UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{Uuid, statsdoubletapselect} ->
			UpdateFun =	fun(Record) -> 
				Tally = Record#apodtelemetry.statsdoubletapselect + 1,
				mnesia:write(Record#apodtelemetry{statsdoubletapselect = Tally})
			end,
			find_client_uuid_and_update(Uuid, UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{Uuid, statslongpresstoselect} ->
			UpdateFun =	fun(Record) -> 
				Tally = Record#apodtelemetry.statslongpresstoselect + 1,
				mnesia:write(Record#apodtelemetry{statslongpresstoselect = Tally})
			end,
			find_client_uuid_and_update(Uuid, UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{Uuid, statszoompan} ->
			UpdateFun =	fun(Record) -> 
				Tally = Record#apodtelemetry.statszoompan + 1,
				mnesia:write(Record#apodtelemetry{statszoompan = Tally})
			end,
			find_client_uuid_and_update(Uuid, UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{Uuid, statssetwallpaper} ->
			UpdateFun =	fun(Record) -> 
				Tally = Record#apodtelemetry.statssetwallpaper + 1,
				mnesia:write(Record#apodtelemetry{statssetwallpaper = Tally})
			end,
			find_client_uuid_and_update(Uuid, UpdateFun),
			#{
				uuid => Uuid,
				registered => true,
				result => <<"success">>
			};

		{_Uuid, statsgetallstats} ->
			#{
				uuid => Uuid,
				registered => true,
				result => fetch_stats()
			};

		{_Uuid, Action} ->
			BadAction = atom_to_binary(Action, utf8),
			#{
				uuid => Uuid,
				registered => true,
				result => [<<"no such action:", BadAction/binary>>]
			}
	end.

find_client_uuid_and_update(Uuid, UpdateFun) ->
    Match = ets:fun2ms(
        fun(Record) 
            when Record#apodtelemetry.uuid =:= Uuid->
                Record
        end),

    case mnesia:transaction(fun() -> mnesia:select(apodtelemetry, Match) end) of
    	{atomic, [Record]} -> 
    			Key = Record#apodtelemetry.uuid,
                UpdateRecord = fun() -> 
	    		case mnesia:read(apodtelemetry, Key) of 
	    			[] -> io:format("Record not found for key: ~p~n", [Key]),
	    			{error, not_found, Key};
	    			[UpdatedRecord] -> 
	    				UpdateFun(UpdatedRecord)
	   		 	end
	   		 end,
	   		 mnesia:transaction(UpdateRecord);
       	{atomic, []} -> 
       		io:format("got nothing for uuid:~p~n", [Uuid]),
       		ok
    end.

%%
%% This function populates a mnesia database with the contests of all
%% files found in [DirName]. Files are assumed to be valid json.
%%
update_db_from_json_file(DirName) ->
	{ok, Filelist} = file:list_dir(DirName),	%generate a list of file names from the directory provided
	process_file_list(DirName, Filelist).		%parse the files and populate DB


%% This function reads a list of files recusively, generates a single list containing
%% all parsed data and calls jiffy to convert the data from string to Json.
%% [DirName] -- the name of the directory. It will be concatinated with the file name
%% [FileName | T] -- a pattern matched list of file names

process_file_list(DirName, [FileName|T]) ->
	FullName = filename:join(DirName, FileName),
    FileData = readlines(FullName),  %% read all data from the file 
    try jiffy:decode(FileData, []) of
    	JsonData ->
		    insert_db_entries(apod, JsonData)					 %% insert the json data into mnesia
    catch
    	Class:Reason ->
    		io:format("~p~n ~p~n ~p~n", [Class, Reason, filename:join(DirName, FileName)])
    after
	    process_file_list(DirName, T) 
    end;

%% Terminating call for the tail recurcive call above. 
process_file_list(_, []) ->
    true.

%%
%% This function inserts [JsonData] into a mnesia database
%% [JsonData] -- well formed json data
%%
insert_db_entries(apod, JsonData) when JsonData =/= [] ->
	Fun = fun() ->	% The function used in a mnesia transaction
		lists:foreach(	%% for each item in the list
		fun({DBEntry}) ->
			Record = create_record(apod, DBEntry), %% convert the item to a struct
			mnesia:write(Record)			%% if this is an image, store it
		end,
		JsonData)
	end,
	mnesia:transaction(Fun); %% execute the transaction

insert_db_entries(youtube, JsonData) ->
	Items =  maps:get(<<"items">>, JsonData),
	extract_item_from_list(Items).

extract_item_from_list([Item|Items]) ->
	Fun = fun() ->
		Record = create_record(youtube, Item), %% convert the item to a struct
		mnesia:write(Record)			%% if this is an image, store it
	end,

	Id = maps:get(<<"id">>, Item),
	case maps:find(<<"videoId">>, Id) of
		error ->
			Etag = maps:get(<<"etag">>, Item),
			io:format("Video id for etag: ~p not found~n", [Etag]);
		_ -> 
			mnesia:transaction(Fun)
	end,
	extract_item_from_list(Items);

extract_item_from_list([]) ->
	{ok, done}.


 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Private Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fetch_stats() -> 
	db_access:dump_telemetry_table().


readlines(FileName) ->
	case file:open(FileName, [read]) of 
		{error, Error} -> io:format("~nError:~p ~p~n",[Error, FileName]);
    {ok, Device} -> 
	    try 
	    	get_all_lines(Device)
	      after 
	      	file:close(Device)
	    end
	end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.

create_record(apod, Item) ->
	#apodimagetable{
				url 			= proplists:get_value(<<"url">>, Item),
				copyright 		= proplists:get_value(<<"copyright">>, Item, <<"no copyright available">>),
				date 			= date_to_gregorian_days(proplists:get_value(<<"date">>, Item)),
				explanation		=	proplists:get_value(<<"explanation">>, Item),
				hdurl			=	proplists:get_value(<<"hdurl">>, Item),
				media_type		=	proplists:get_value(<<"media_type">>, Item),
				service_version	=	proplists:get_value(<<"service_version">>, Item),
				title 			=	proplists:get_value(<<"title">>, Item)
				};

create_record(youtube, Item) ->
	Id = maps:get(<<"id">>, Item),
	Snippet = maps:get(<<"snippet">>, Item),
	Thumbnails = maps:get(<<"thumbnails">>, Snippet),
	UrlDescriptor = maps:get(<<"medium">>, Thumbnails),
	try maps:get(<<"videoId">>, Id) of
		VideoID ->
			Part1 = maps:get(<<"channelId">>, Snippet),
			Part2 = VideoID,
			Key = <<Part1/binary,<<":">>/binary, Part2/binary>>,
			#youtube_channel{
					key 			= Key,
					channel_id 		= maps:get(<<"channelId">>, Snippet),
					video_id 		= VideoID,
					date 			= date_to_gregorian_days(maps:get(<<"publishTime">>, Snippet)),
					width			=	maps:get(<<"width">>, UrlDescriptor),
					height			=	maps:get(<<"height">>, UrlDescriptor),
					title			=	maps:get(<<"title">>, Snippet),
					url_medium		=	maps:get(<<"url">>, UrlDescriptor)
					}
	catch _:_ ->
			io:format("Badkey for record: ~p~n", [Id])
	end.


find_token_in_string(Heystack, [Needle|ListOfNeedles]) ->
	case string:find(Heystack, Needle) of
		nomatch ->
			find_token_in_string(Heystack, ListOfNeedles);
		_ ->
			io:format("Found ~p~n", [Needle]),
			true
	end;
find_token_in_string(_Args, []) -> false.

generate_coparable_list(ListOfBinaries) ->
	list_of_binaries_to_lower_case_list_of_binaries(ListOfBinaries, []).

list_of_binaries_to_lower_case_list_of_binaries([H|T], Acc) ->
	list_of_binaries_to_lower_case_list_of_binaries(T, [string:casefold(H)|Acc]);
	
list_of_binaries_to_lower_case_list_of_binaries([], Acc) -> Acc.

reformat_channel_data(Map) ->
	ChannelMap = maps:get(?TOP_KEY, Map),
	Channels = maps:keys(ChannelMap),
	process_single_channel(Channels, ChannelMap, []).

process_single_channel([Channel|Channels], ChannelMap, Acc) ->
	ChannelMapData = maps:get(Channel, ChannelMap),
	Pages = maps:keys(ChannelMapData),
	ListOfItems = process_channel_data(Pages, ChannelMapData, []),
	process_single_channel(Channels, ChannelMap, lists:append(Acc, ListOfItems));

process_single_channel([], _ChannelMap, Acc) -> Acc.

process_channel_data([Page|Pages], ChannelMapData, Acc) ->
	PageMap = maps:get(Page, ChannelMapData),
	Items = maps:get(?YOUTUBE_VIDEO_ARRAY_KEY, PageMap),
	ItemsMap = extract_items(Items, Acc),
	process_channel_data(Pages, ChannelMapData, ItemsMap);


process_channel_data([], _ChannelMapData, Acc) ->
	Acc.

extract_items([Item | Items], Acc) ->
	Snippet = maps:get(<<"snippet">>, Item),
	Id = maps:get(<<"id">>, Item),
	Thumbnails = maps:get( <<"thumbnails">>, Snippet),
	UrlData = maps:get(<<"medium">>, Thumbnails),
 	Date = maps:get(<<"publishTime">>, Snippet),
 	case maps:is_key(<<"videoId">>, Id) of
 		true ->
		    DBItem = #{
			    channel_id    => maps:get(<<"channelId">>, Snippet),
			    video_id      => maps:get(<<"videoId">>, Id),
			    url_medium    => maps:get(<<"url">>, UrlData),
			    width         => maps:get(<<"width">>, UrlData),
			    height        => maps:get(<<"height">>, UrlData),
			    title         => maps:get(<<"title">>, Snippet),
			    date          => utils:date_to_gregorian_days(Date)
			},
			extract_items(Items, lists:append(Acc, [DBItem]));
		_ ->
			extract_items(Items, Acc)
 	end;

extract_items([], Acc) ->
	Acc.                                 

compose_error_message(ErrorCode, ErrorMessage) ->
	{Date, Time} = calendar:now_to_local_time(erlang:timestamp()),
	Timestamp  = calendar:datetime_to_gregorian_seconds({Date, Time}),
	{[{timestamp, Timestamp}, {errorcode, ErrorCode}, {message, ErrorMessage}]}.

jsonify_client_profile_table(MapOfRecords) ->
	ListOfKeys = maps:keys(MapOfRecords), %% get a list of records.
	RecordExtractor = fun(Key, MapAcc) ->
		[Record] = maps:get(Key, MapOfRecords),
		#client_profile_table{
			client_id = _ClientID,
			channel_list = ClientChannels
		} = Record,

		ListExtractor = fun(Item, ListAcc) ->
			{ChannelName,[{youtubekey,YoutubeKey},{channel_id,ChannelID}]} = Item,
			Map = #{
				channel_name => ChannelName,
				youtubekey => YoutubeKey,
	        	channel_id => ChannelID
			},
			lists:append(ListAcc, [Map])
		end,
		List = lists:foldr(ListExtractor, [], ClientChannels),
		maps:put(Key, List, MapAcc)
	end,
	lists:foldr(RecordExtractor, #{}, ListOfKeys).

jsonify_list_of_tuples(NamesList, TupleList) ->
	% NameList must contain the same number of items as each tuple in the TupleList
	[String] = jsonify_items(NamesList, TupleList, []),
	String.

jsonify_items(_Arg1, [], Acc) -> 
	lists:reverse(Acc);
jsonify_items(NamesList, [Tuple|Tail], Acc) ->
	Item = create_tuple(NamesList, Tuple, 1, #{}),
	NewList = lists:append(Acc,[Item]),
	jsonify_items(NamesList, Tail, NewList).

create_tuple([], _Tuple, _Count, Acc) -> Acc;
create_tuple([Name|Tail], Tuple, Count, Acc) ->
	Map = maps:put(Name, element(Count, Tuple), Acc),
	create_tuple(Tail, Tuple, Count+1, Map).
format_error(ErrorType, ErrorMessage) ->
	 Error = #{
		date_time => utils:current_time_string(),
		error_code => -1,
		error_text => <<ErrorType/binary, ErrorMessage/binary>>
	},
	{error, Error}.

package_channel_record_list(RecordList) ->
	List = package_records(RecordList, []),
	{ok, jiffy:encode(#{videos => List})}.

package_records([], Acc) -> Acc;
package_records([Record|Records], List) ->
	A = maps:put(width, Record#youtube_channel.width, #{}),
	B = maps:put(video_id, Record#youtube_channel.video_id, A),
	C = maps:put(url_medium, Record#youtube_channel.url_medium, B),
	D = maps:put(title, Record#youtube_channel.title, C),
	E = maps:put(height, Record#youtube_channel.height, D),
	F = maps:put(date, Record#youtube_channel.date, E),
	Final = maps:put(channel_id, Record#youtube_channel.channel_id, F),
	NewList = lists:append(List, [Final]),
	package_records(Records, NewList).

%%%%%%%%%%%%%%%%%%%%% DEBUG CODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_test_json(ChannelKey, JsonData) ->
	ChannelData = maps:get(ChannelKey, JsonData),
	Pages = maps:keys(ChannelData),
	Fun = fun(Page) -> 
		VideoIdData = maps:get(Page, ChannelData),
		insert_db_entries(youtube, VideoIdData)
	end,
	lists:foreach(Fun, Pages).

write_test_data_to_db_and_dump_to_file() ->
	{ok, FileData} = file:read_file("/home/oleg/temp/multi_channel_test.json"),
	JsonData = jiffy:decode(FileData, [return_maps]),
	ChannelKeys = maps:keys(JsonData),
	Fun = fun(ChannelKey) -> 
		parse_test_json(ChannelKey, JsonData)
	end,
	lists:foreach(Fun, ChannelKeys),

	mnesia:dump_to_textfile("/home/oleg/temp/mnesia_db.txt").

test_multi_channel_data_fetch() ->
	{ok, FileData} = file:read_file("/home/oleg/temp/multi_channel_test.json"),
	JsonData = jiffy:decode(FileData, [return_maps]),
	ChannelKeys = maps:keys(JsonData),
	Key1 = lists:nth(1, ChannelKeys),
	Key2 = lists:nth(2, ChannelKeys),
	MasterMap = #{},
	Map1a = maps:put(Key1, maps:get(Key1, JsonData), MasterMap),
	Map2a = maps:put(Key2, maps:get(Key2, JsonData), MasterMap),
	MergedMap = maps:merge(Map1a, Map2a),
	JsonResult = jiffy:encode(MergedMap),
	io:format("MergedMap: ~p~n", [JsonResult]).