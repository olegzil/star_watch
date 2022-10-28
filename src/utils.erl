% @Author: Oleg Zilberman
% @Date:   2022-10-08 13:34:16
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-10-28 16:23:42
-module(utils).
-export([date_to_gregorian_days/1, 
		 gregorian_days_to_binary/1, 
		 fetch_apod_data/3, 
		 fetch_data/1,
		 current_time_string/0,
		 start_cron_job/0,
		 time_pair_to_fetch/2,
		 update_client_record/1, 
		 dump_telemetry_table/0,
		 process_file_list/2,
		 update_db_from_json_file/1,
		 insert_apod_entries/1, 
		 update_database/1]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("include/apodtelemetry.hrl").
-include("include/apod_record_def.hrl").

-define(ROOT_HOST, "https://api.nasa.gov/planetary/apod?").
-define(API_KEY, "K9jqPfqphwz3s1BsTbPQjsi2c4kn4eV7wBFh2MR8").


date_to_gregorian_days(Date) ->
    DateTuple = list_to_tuple(lists:map(fun(Item)-> binary_to_integer(Item) end, string:split(Date, "-", all))),
    calendar:date_to_gregorian_days(DateTuple).

gregorian_days_to_binary(Date) ->
	GregorianDate = calendar:gregorian_days_to_date(Date),
	List = tuple_to_list(GregorianDate), 
	ListOfStrings = lists:map(fun(Item)-> integer_to_list(Item) end, List),
	list_to_binary(string:join(ListOfStrings, "-")).

update_database(Data) ->
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
	fetch_apod_data(Past, Future, production);	

fetch_data(periodic) ->
	Past = 24*60*60,
	Future = 0,
	fetch_apod_data(Past, Future, production).

fetch_apod_data(Past, Future, production) ->
	Query = uri_string:compose_query([{"start_date", time_pair_to_fetch(past, Past)}, 
									  {"end_date", time_pair_to_fetch(future, Future)},
									  {"api_key", ?API_KEY},
									  {"thumbs", "true"}
									  ]),
	Request = string:join([?ROOT_HOST, Query], ""),
	case httpc:request(Request) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			update_database(Body);
		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end;

fetch_apod_data(GregorianStartDays, GregorianEndDays, notfound) ->
	S = calendar:gregorian_days_to_date(GregorianStartDays),
	E = calendar:gregorian_days_to_date(GregorianEndDays),
	fetch_apod_data(S, E, tuples);

fetch_apod_data({StartYear, StartMonth, StartDay}, {EndYear, EndMonth, EndDay}, tuples) ->
	Past = binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p-~.2.0p-~.2.0p",[StartYear, StartMonth, StartDay]))),
	Future = binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p-~.2.0p-~.2.0p",[EndYear, EndMonth, EndDay]))),
	Query = uri_string:compose_query([{"start_date", Past}, 
									  {"end_date", Future},
									  {"api_key", ?API_KEY},
									  {"thumbs", "true"}
									  ]),
	Request = string:join([?ROOT_HOST, Query], ""),
	case httpc:request(Request) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			update_database(Body),
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
	Job = {{daily, {12, 1, am}},
    {utils, fetch_data, [periodic]}},

    erlcron:cron(apod_daily_fetch_job, Job).

update_client_record(Telemetry) ->
	case Telemetry of 
		{ok, Uuid} ->
			find_client_ip_and_update(Uuid);
		{_, Error} ->
			io:format("~p~n", [Error])
	end.

find_client_ip_and_update(IpAddress) ->
    Match = ets:fun2ms(
        fun(Record) 
            when Record#apodtelemetry.ip_address =:= IpAddress->
                Record
        end),

    case mnesia:transaction(fun() -> mnesia:select(apodtelemetry, Match) end) of
        {atomic,[]} ->
            mnesia:transaction(
            	fun() -> 
            		mnesia:write(#apodtelemetry{ip_address = IpAddress, access_tally = 1})
            	end
           	);
        {atomic,[{_, Key,_}]} ->
        	UpdateRecord = fun() -> 
        		case mnesia:read(apodtelemetry, Key) of 
        			[] -> io:format("Unexpected. Record not found ~p", [IpAddress]);
        			[#apodtelemetry{ip_address = IpAddress, access_tally = Tally}] ->
	            		mnesia:write(#apodtelemetry{ip_address = IpAddress, access_tally = Tally + 1}),
	            		mnesia:read(apodtelemetry, Key)
       		 	end
       		 end,
       		 mnesia:transaction(UpdateRecord)
    end.

dump_telemetry_table() ->
	Fun = fun(#apodtelemetry{ip_address = IpAddress, access_tally = Tally}, Acc) ->
		% io:format("Ip = ~p Tally = ~p~n", [IpAddress, Tally]),
		lists:append(Acc, [{IpAddress, Tally}])
	end, 
	Transaction = fun() ->
	  mnesia:foldr(Fun, [], apodtelemetry)
	end,
	{atomic, Records} = mnesia:transaction(Transaction),
	io:format("~p~n", [Records]).

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

