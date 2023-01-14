% @Author: Oleg Zilberman
% @Date:   2023-01-12 10:11:59
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-12 19:40:36
-module(nasa_rest_access).
-include("include/macro_definitions.hrl").
-export([fetch_root_page/3]).

fetch_root_page(CelestialBodyName, GregorianStartDays, GregorianEndDays) ->
	{StartYear, _StartMonth, _StartDay} = calendar:gregorian_days_to_date(GregorianStartDays), 
	{EndYear, _EndMonth, _EndDay} = calendar:gregorian_days_to_date(GregorianEndDays),
	Past = binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p",[StartYear]))),
	Future = binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p",[EndYear]))),
	Query = uri_string:compose_query([{"q", CelestialBodyName},
									  {"year_start", Past}, 
									  {"year_end", Future}
									  ]),
	Request = string:join([?NASA_IMAGES_HOST, Query], ""),
	case httpc:request(Request) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			FilteredData = parse_root_page(Body),
			FinalDBData = fetch_collection_json(FilteredData),
			utils:update_database(nasadata, FinalDBData),
			{ok, Body};
		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end.

parse_root_page(RootPage) ->
	%% TODO:Implement
	%% Convert to json
	%% Use filter to create a list that contains only data of interest
	%% Traverse the new json package and extract date, description, title, thumb-image and href
	%% Return result of last traversal as {ok, Data} or {error, Error}
	ok.

fetch_collection_json(CelestialObjectList) ->
	%% TODO:Implement
	%% Extract collection.json URL to fetch the data
	%% Success: Extract name that contains the string "-orig" and update the input list
	%% Failure: Update input list with thumb URL and continue
	%% Return list
	ok.

