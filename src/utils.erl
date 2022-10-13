% @Author: Oleg Zilberman
% @Date:   2022-10-08 13:34:16
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-10-12 17:53:28
-module(utils).
-export([date_to_gregorian_days/1, 
		 gregorian_days_to_binary/1, 
		 fetch_apod_data/2, 
		 fetch_data/1,
		 current_time_string/0,
		 start_cron_job/0,
		 time_pair_to_fetch/2]).

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
		    io:format("~p~n", [db_access:insert_apod_entries(JsonData)])

    catch
    	Class:Reason ->
    		io:format("~p~n ~p~n", [Class, Reason])
    end.

fetch_data(now) ->
	Future = 0,
	Past = 0,
	fetch_apod_data(Past, Future);	

fetch_data(periodic) ->
	Past = 24*60*60,
	Future = 0,
	fetch_apod_data(Past, Future).

fetch_apod_data(Past, Future) ->
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
			io:format("Error: ~p~n", [jiffy:decode(ErrorMessage)]);
		Other ->
			io:format("Unknown: ~p~n", [Other])
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
