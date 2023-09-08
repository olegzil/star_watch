% @Author: Oleg Zilberman
% @Date:   2022-10-08 13:34:16
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-31 16:58:16
-module(utils).
-export([v4/0, to_string/1, get_parts/1]).
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
		 format_success/2,
		 jsonify_list_of_tuples/2, 
		 jsonify_client_profile_table/1,
		 format_error/2,
		 package_channel_record_list/1,
		 is_key_present/3,
		 tuple_list_to_list_of_maps/2,
		 config_records_to_list_of_maps/2,
		 remove_duplicate_channels/0,
		 log_message/1,
		 decrypt/2,
		 select_pid/1,
		 make_nonstring_binary/1,
		 decrypt_data/1,
		 format_login_server_return/3,
		 extract_id_and_password/1,
		 extract_id/1,
		 exec_extern_cmd/1,
		 get_current_endpoints/0]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("include/apodtelemetry.hrl").
-include("include/apod_record_def.hrl").
-include("include/youtube_channel.hrl").
-include("include/macro_definitions.hrl").
-include("include/client_profile_table.hrl").
-include("src/include/registration_query.hrl").
-include("include/users_login_table.hrl").

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
    end;

update_database(video_link, Data) ->
    try jiffy:decode(Data, [return_maps]) of
    	Map ->
		    %% insert the json data into mnesia
    		insert_db_entries(video_link, Map),
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
	Items =  maps:get(?YOUTUBE_VIDEO_ARRAY_KEY, JsonData),
	extract_item_from_list(Items);

insert_db_entries(video_link, JsonData) ->
	Items =  maps:get(?YOUTUBE_VIDEO_ARRAY_KEY, JsonData),
	ItemMap = lists:nth(1, Items),
	Fun = fun() ->
		Record = create_record(youtube_link, ItemMap), %% convert the item to a struct
		mnesia:write(Record)			%% if this is an image, store it
	end,
	mnesia:transaction(Fun).

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
	end;

create_record(video_link, ItemMap) ->
	SnippetMap = maps:get(<<"snippet">>, ItemMap),
	ThumbnailMap = maps:get(<<"thumbnails">>, SnippetMap),
	DimentionsMap = maps:get(<<"standard">>, ThumbnailMap),
	VideoID = maps:get(<<"id">>, SnippetMap),
	Date = maps:get(<<"publishedAt">>, SnippetMap),
	Title = maps:get(<<"title">>, SnippetMap),
	ChannelID = maps:get(<<"channelId">>, SnippetMap),
	Thumbnail = maps:get(<<"url">>, DimentionsMap),
	Width = maps:get(<<"width">>, DimentionsMap),
	Height = maps:get(<<"height">>, DimentionsMap),
	Key = <<ChannelID/binary,<<":">>/binary, VideoID/binary>>,
	#youtube_channel{
			key 			= Key,
			channel_id 		= ChannelID,
			video_id 		= VideoID,
			date 			= date_to_gregorian_days(Date),
			width			=	Width,
			height			=	Height,
			title			=	Title,
			url_medium		=	Thumbnail
			}.

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

jsonify_client_profile_table(Record) ->
	#{
		client_id => Record#client_profile_table.client_id,
		youtube_key => Record#client_profile_table.youtube_key,
		channel_list => Record#client_profile_table.channel_list
	}.

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
format_error(ErrorCode, ErrorMessage) ->
	Message = if
				is_atom(ErrorMessage) ->
					atom_to_binary(ErrorMessage);
				is_binary(ErrorMessage) ->
					ErrorMessage;
				true ->
					io_lib:format("~p", [ErrorMessage])
			end,
	 Error = #{
	 	status => error,
		date_time => utils:current_time_string(),
		error_code => ErrorCode,
		error_text => list_to_binary(io_lib:format("~p hex code: ~.16B", [Message, ErrorCode]))
	},
	{error, Error}.

format_success(Code, Message) ->
	Message = if
				is_atom(Message) ->
					atom_to_binary(Message);
				is_binary(Message) ->
					Message;
				true ->
					<<"undefined">>
			end,
	 Success = #{
	 	code => Code,
		date_time => utils:current_time_string(),
		text => Message
	},
	{ok, Success}.

format_login_server_return(Code, Type, Message) ->
	if
		erlang:is_map(Type) =:= true ->
			{ok, #{
				    user_id => maps:get(user_id, Type),
				    client_id => maps:get(client_id, Type),
				    log_in_state => Code,
				    log_in_time => maps:get(log_in_time, Type),
				    error_text => Message,
				    user_validated => maps:get(user_validated, Type)
				}};
		erlang:is_record(Type, users_login_table) =:= true ->
			{ok, #{
				    user_id => Type#users_login_table.user_id,
				    client_id => Type#users_login_table.client_id,
				    log_in_state => Code,
				    log_in_time => Type#users_login_table.log_in_time,
				    error_text => Message,
				    user_validated => Type#users_login_table.user_validated
				}};
		true -> error
	end.

package_channel_record_list(RecordList) ->
	List = package_records(RecordList, []),
	{ok, jiffy:encode(#{videos => List})}.

package_records([], Acc) -> Acc;
package_records([Record|Records], List) ->
	RecordList = [{width, Record#youtube_channel.width}, 
				  {video_id, Record#youtube_channel.video_id}, 
				  {url_medium, Record#youtube_channel.url_medium}, 
				  {title, Record#youtube_channel.title}, 
				  {height, Record#youtube_channel.height}, 
				  {date, utils:gregorian_days_to_binary(Record#youtube_channel.date)}, 
				  {channel_id, Record#youtube_channel.channel_id}],

	Final = maps:from_list(RecordList),
	NewList = lists:append(List, [Final]),
	package_records(Records, NewList).

log_message(Items) ->
	Result = lists:foldl(fun(Item, Acc)-> 
		{Title, Var} = Item,
		{Description, Values} = Acc,
		A = list_to_binary(Title),
		NewDescription = <<Description/binary, A/binary, ": ~p~n">>,
		NewValues = Values ++ [Var],
		{NewDescription, NewValues}
		end, 
		{<<"">>, []}, 
		Items),
	{Caption, Values} = Result,
	io:format("~n***********************~n"),
	io:format(Caption, Values),
	io:format("***********************~n").

is_key_present(Key, Index, List) ->
	case lists:keyfind(Key, Index, List) of
		false ->
			false;
		_ ->
			true
	end.
%%%
%% Given a tupe of {Name1, Name2} and a list of tuples [{part1, part2} ... {partN, partN+1}] 
%% returns a list of maps: [#{Name1 => part1, Name2 => part2} ... #{Name1 => partN, Name2 => partN+1}]
%%% 
tuple_list_to_list_of_maps({Tag1, Tag2}, ListOfTuples) ->
	lists:foldl(fun(Tuple, Acc)-> 
		Map = #{Tag1 => element(1, Tuple), Tag2 => element(2, Tuple)},
		Acc ++ [Map]
	end, [], ListOfTuples).


config_records_to_list_of_maps(Keys, MapOfRecords) ->
	lists:foldl(fun(Key, Acc)-> 
		[R] = maps:get(Key, MapOfRecords),
		RecordMap = #{
			client_id => Key,
			youtube_key => R#client_profile_table.youtube_key,
			channel_list => utils:tuple_list_to_list_of_maps({channel_name, channel_id}, R#client_profile_table.channel_list)
		},				
		Acc ++ [RecordMap]
	end,
	[], Keys).

select_pid({_ChildID, {_ChildStartResults, Pid}}) ->
	Pid;
select_pid({_Ref,Pid}) ->
	Pid;
select_pid(_) ->
	error.

decrypt(Key, EncryptedData) ->
	Data = binary:part(EncryptedData, {1, byte_size(EncryptedData)-2}),
	Binary = make_nonstring_binary(Data),
    DecryptedData = public_key:decrypt_private(Binary, Key),
    {ok, DecryptedData}.

extract_id(Data) ->
    {ok, Id} = utils:decrypt_data(Data),
    {ok, Id}.

extract_id_and_password(Data) ->
	extract_id_and_password(start, Data).

extract_id_and_password(start, Data) ->
    {ok, ClearText} = utils:decrypt_data(Data),
    Parts = string:split(ClearText, ?LOGIN_ID_TOKEN),
    extract_id_and_password(validate_part1, Parts);

extract_id_and_password(validate_part1, Data) when length(Data) =:= 2 ->
    extract_id_and_password(validate_part2, Data);

extract_id_and_password(validate_part1, _Data) -> false;
extract_id_and_password(validate_part2, [_,Data]) ->
    Parts = string:split(Data, ?LOGIN_PASSWORD_TOKEN),
    extract_id_and_password(validate_part3, Parts);

extract_id_and_password(validate_part3, Data) when length(Data) =:= 2 ->
    [ID, Password] = Data,
    {string:lowercase(string:trim(ID)), string:trim(Password)};

extract_id_and_password(validate_part3, _Data) -> false.


make_nonstring_binary(Data) ->
	List = lists:map(fun(X) -> list_to_integer(X) end, string:tokens(binary_to_list(Data), ", ")),
	[First|Tail] = List,
    make_nonstring_binary(Tail, <<First>>).

make_nonstring_binary([H|T], Acc) ->
    make_nonstring_binary(T, <<Acc/binary, H>>);
make_nonstring_binary([], Acc) ->
    Acc.

decrypt_data(EncryptedData) ->
    {ok, PemBin} = server_config_processor:read_private_key_file(),
    [RSAEntry] = public_key:pem_decode(PemBin),
    Key = public_key:pem_entry_decode(RSAEntry),
    case utils:decrypt(Key, EncryptedData) of
        {ok, ClearText} ->
            {ok, ClearText};
        {error, Error} ->
            {error, Error}
    end.


% Generates a random binary UUID.
v4() ->
  v4(rand:uniform(round(math:pow(2, 48))) - 1, rand:uniform(round(math:pow(2, 12))) - 1, rand:uniform(round(math:pow(2, 32))) - 1, rand:uniform(round(math:pow(2, 30))) - 1).

% Origina function. Removed due to deprication
% v4() ->
%   v4(crypto:rand_uniform(1, round(math:pow(2, 48))) - 1, crypto:rand_uniform(1, round(math:pow(2, 12))) - 1, crypto:rand_uniform(1, round(math:pow(2, 32))) - 1, crypto:rand_uniform(1, round(math:pow(2, 30))) - 1).

v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

% Returns a string representation of a binary UUID.
to_string(U) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).

% Returns the 32, 16, 16, 8, 8, 48 parts of a binary UUID.
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].

%%%%%%%%%%%%%%%%%%%%% DEBUG CODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_current_endpoints() ->
	case server_config_processor:get_ip_flag(?SERVER_CONFIG_FILE) of 
		true ->
			{0, Result} = exec_extern_cmd("ifconfig"),
			Start = string:rstr(Result, "wlp3s0:"),
			Substr = string:sub_string(Result, Start),
			Tokens = string:tokens(Substr, " "),
			IPValue = list_to_binary(lists:nth(6, Tokens)),
			EndPoint = <<"http://", IPValue/binary, ":", ?HTTP_PORT_LOCAL/binary, "/">>,
			utils:log_message([{"IPValue", IPValue}, {"active_port", ?HTTP_PORT_LOCAL}, {"call_back_endpoint", EndPoint}]),
			#{
				ip_value => IPValue,
				active_port => binary_to_integer(?HTTP_PORT_LOCAL),
				call_back_endpoint => EndPoint
			};
		false ->
			#{
				ip_value => ?REMOTE_IP,
				active_port => binary_to_integer(?HTTP_PORT_REMOTE),
				call_back_endpoint => ?LOGIN_CALLBACK_ADDRESS_REMOTE
			}
	end.

exec_extern_cmd(Command) ->
    Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
    get_data(Port, []).

get_data(Port, Sofar) ->
    receive
    {Port, {data, Bytes}} ->
        get_data(Port, [Sofar|Bytes]);
    {Port, eof} ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            true
        end,
        receive
        {'EXIT',  Port,  _} ->
            ok
        after 1 ->              % force context switch
            ok
        end,
        ExitCode =
            receive
            {Port, {exit_status, Code}} ->
                Code
        end,
        {ExitCode, lists:flatten(Sofar)}
    end.

remove_duplicate_channels() ->
	{atomic, Keys} = mnesia:transaction(fun() -> mnesia:all_keys(client_profile_table) end),
	lists:foreach(fun(Key)-> 
		{atomic, [Record]} = mnesia:transaction(fun() -> mnesia:read(client_profile_table, Key) end),
		remove_duplicate_channels(worker, Record)
	end,
	Keys).

remove_duplicate_channels(worker, Record) ->
	Unique = lists:uniq(fun({_, ChannelID})-> ChannelID end, Record#client_profile_table.channel_list),
	UpdatedRecord = Record#client_profile_table{
	channel_list = Unique
	},
	mnesia:transaction(fun() -> mnesia:write(UpdatedRecord) end).

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