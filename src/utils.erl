% @Author: Oleg Zilberman
% @Date:   2022-10-08 13:34:16
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-29 20:51:07
-module(utils).
-export([date_to_gregorian_days/1, 
		 gregorian_days_to_binary/1, 
		 fetch_apod_data/3, 
		 fetch_data/1,
		 current_time_string/0,
		 start_cron_job/0,
		 time_pair_to_fetch/2,
		 update_client_record/1, 
		 process_file_list/2,
		 update_db_from_json_file/1,
		 insert_apod_entries/1, 
		 update_database/2,
		 find_token_in_string/2,
		 generate_coparable_list/1,
		 convert_date_to_integer/0,
		 encode_url/1]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("include/apodtelemetry.hrl").
-include("include/apod_record_def.hrl").
-include("include/macro_definitions.hrl").
-include("include/celestial_object_table.hrl").
-include("src/include/registration_query.hrl").

%%% DateString -- <<"2018-11-26T00:00:00Z">>
%%% 1. Use string:split to create a list of strings/binaries
%%% 2. Use string:split again to separate the time string from the letter 'T' and generate [Day, Time]
%%% 3. Generate a Date tuple
%%% 4. Finally call date_to_gregorain_days to get an integer representing the time span in days
date_to_gregorian_days([DateString]) ->
	[Year, Month, DateTime] = string:split(DateString, "-", all),
	[Day, _Time] = string:split(DateTime, "T"),
	Date = {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
	calendar:date_to_gregorian_days(Date);


date_to_gregorian_days(DateString) ->
	[Year, Month, Day] = string:split(DateString, "-", all),
	Date = {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
	calendar:date_to_gregorian_days(Date).

% date_to_gregorian_days(<<A,B,C,D,E,F,G,H,I,J>>) ->
% 	Date = <<A,B,C,D,E,F,G,H,I,J>>,
%     DateTuple = list_to_tuple(lists:map(fun(Item)-> binary_to_integer(Item) end, string:split(Date, "-", all))),
%     calendar:date_to_gregorian_days(DateTuple);

% date_to_gregorian_days(<<A,B,C,D>>) ->
% 	Date = <<A,B,C,D>>,
%     Tuple = list_to_tuple(lists:map(fun(Item)-> binary_to_integer(Item) end, string:split(Date, "", all))),
%     DateTuple = erlang:insert_element(2,erlang:insert_element(2, Tuple, 1), 1),
%     calendar:date_to_gregorian_days(DateTuple).

gregorian_days_to_binary(Date) ->
	GregorianDate = calendar:gregorian_days_to_date(Date),
	List = tuple_to_list(GregorianDate), 
	ListOfStrings = lists:map(fun(Item)-> integer_to_list(Item) end, List),
	list_to_binary(string:join(ListOfStrings, "-")).

update_database(apod, Data) ->
    try jiffy:decode(Data, []) of
    	JsonData ->
		    %% insert the json data into mnesia
		    io:format("~p~n", [insert_apod_entries(JsonData)])

    catch
    	Class:Reason ->
    		io:format("~p~n ~p~n", [Class, Reason])
    end.

fetch_data(now) ->
	Future = 0,
	Past = 0,
	fetch_apod_data(production, Past, Future);	

fetch_data(periodic) ->
	Past = 24*60*60,
	Future = 0,
	fetch_apod_data(production, Past, Future).

fetch_apod_data(production, Past, Future) ->
	Query = uri_string:compose_query([{"start_date", time_pair_to_fetch(past, Past)}, 
									  {"end_date", time_pair_to_fetch(future, Future)},
									  {"api_key", ?API_KEY},
									  {"thumbs", "true"}
									  ]),
	Request = string:join([?APOD_HOST, Query], ""),
	case httpc:request(Request) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			update_database(apod, Body);
		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end;

fetch_apod_data(notfound, GregorianStartDays, GregorianEndDays) ->
	S = calendar:gregorian_days_to_date(GregorianStartDays),
	E = calendar:gregorian_days_to_date(GregorianEndDays),
	fetch_apod_data(tuples, S, E);

fetch_apod_data(tuples, {StartYear, StartMonth, StartDay}, {EndYear, EndMonth, EndDay}) ->
	Past = binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p-~.2.0p-~.2.0p",[StartYear, StartMonth, StartDay]))),
	Future = binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p-~.2.0p-~.2.0p",[EndYear, EndMonth, EndDay]))),
	Query = uri_string:compose_query([{"start_date", Past}, 
									  {"end_date", Future},
									  {"api_key", ?API_KEY},
									  {"thumbs", "true"}
									  ]),
	Request = string:join([?APOD_HOST, Query], ""),
	case httpc:request(Request) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			update_database(apod, Body),
			{ok, Body};
		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
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

start_cron_job() ->
	ImageOfTheDayJob = {{daily, {12, 1, am}},
    {utils, fetch_data, [periodic]}},

    erlcron:cron(apod_daily_fetch_job, ImageOfTheDayJob).

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
			io:format("in isregistered~n"),
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
		    				io:format("record found: ~p~n", [Uuid]),
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
		    insert_apod_entries(JsonData)					 %% insert the json data into mnesia
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
insert_apod_entries(JsonData) when JsonData =/= [] ->
	Fun = fun() ->	% The function used in a mnesia transaction
		lists:foreach(	%% for each item in the list
		fun({ApodEntry}) ->
			Record = from_string_to_json_apod(ApodEntry), %% convert the item to a struct
			mnesia:write(Record)			%% if this is an image, store it
		end,
		JsonData)
	end,
	mnesia:transaction(Fun). %% execute the transaction

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

from_string_to_json_apod(Item) ->
	#apodimagetable{
				url 			= proplists:get_value(<<"url">>, Item),
				copyright 		= proplists:get_value(<<"copyright">>, Item, <<"no copyright available">>),
				date 			= date_to_gregorian_days(proplists:get_value(<<"date">>, Item)),
				explanation		=	proplists:get_value(<<"explanation">>, Item),
				hdurl			=	proplists:get_value(<<"hdurl">>, Item),
				media_type		=	proplists:get_value(<<"media_type">>, Item),
				service_version	=	proplists:get_value(<<"service_version">>, Item),
				title 			=	proplists:get_value(<<"title">>, Item)
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

convert_date_to_integer() ->
    Match = ets:fun2ms(
        fun(Record) when is_binary(Record#celestial_object_table.date) ->
        	Record
        end),
   case mnesia:transaction(fun() -> mnesia:select(celestial_object_table, Match) end) of
    	{atomic, [Record]} -> 
             UpdateRecord = fun() -> 
	        		mnesia:write(Record#celestial_object_table{date = utils:date_to_gregorian_days([Record#celestial_object_table.date])})
	   		 end,
	   		 mnesia:transaction(UpdateRecord);
       	{atomic, []} -> ok
    end.

encode_url(Url) ->
	[Head|Tail] = string:split(Url, " "),
	case Tail of
		[] ->
			Url;
		_ ->
			A = Head,
			[B] = Tail,
			Space = <<" ">>,
			Remainder = <<Space/binary, B/binary>>,
			string:concat(binary_to_list(A),  uri_string:quote(binary_to_list(Remainder)))
	end.
