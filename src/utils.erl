% @Author: Oleg Zilberman
% @Date:   2022-10-08 13:34:16
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-10-11 15:18:40
-module(utils).
-export([date_to_gregorian_days/1, gregorian_days_to_binary/1, fetch_apod_data/0, current_time_string/0]).
date_to_gregorian_days(Date) ->
    DateTuple = list_to_tuple(lists:map(fun(Item)-> binary_to_integer(Item) end, string:split(Date, "-", all))),
    calendar:date_to_gregorian_days(DateTuple).

gregorian_days_to_binary(Date) ->
	GregorianDate = calendar:gregorian_days_to_date(Date),
	List = tuple_to_list(GregorianDate), 
	ListOfStrings = lists:map(fun(Item)-> integer_to_list(Item) end, List),
	list_to_binary(string:join(ListOfStrings, "-")).

fetch_apod_data() ->
	inets:start(),
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
      httpc:request("https://api.nasa.gov/planetary/apod?start_date=1996-6-16&end_date=1996-9-14&api_key=K9jqPfqphwz3s1BsTbPQjsi2c4kn4eV7wBFh2MR8&thumbs=true").	

current_time_string() ->
	{{Year, Month, Day}, {Hour, Min, Sec}} = 
	calendar:now_to_datetime(erlang:timestamp()),
	list_to_binary(io_lib:format("~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.0+00:00", 
		[Year, Month, Day, Hour, Min, Sec])).

