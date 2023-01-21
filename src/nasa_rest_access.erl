% @Author: Oleg Zilberman
% @Date:   2023-01-12 10:11:59
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-20 17:47:28
-module(nasa_rest_access).
-include("include/macro_definitions.hrl").
-include("include/celestial_object_table.hrl").
-export([fetch_root_page/4, parse_nasa_data_update_db/2]).

%%% This function makes an http call to retrive the json for the celestial object and page specified
fetch_root_page(CelestialObject, GregorianStartDays, GregorianEndDays, Page) ->
	{StartYear, _StartMonth, _StartDay} = calendar:gregorian_days_to_date(GregorianStartDays), 
	{EndYear, _EndMonth, _EndDay} = calendar:gregorian_days_to_date(GregorianEndDays),
	Past = binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p",[StartYear]))),
	Future = binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p",[EndYear]))),
	TargetTuple = lists:keyfind(list_to_binary(CelestialObject), 2, ?CELESTIAL_OBJECTS),

	Verb = atom_to_list(element(1, TargetTuple)),
	Subject = element(2, TargetTuple),
	io:format("************** Verb: ~p Subject: ~p~n", [Verb, Subject]),
	Query = uri_string:compose_query([{Verb, Subject},
									  {"year_start", Past}, 
									  {"year_end", Future},
									  {"page", integer_to_list(Page)}
									  ]),
	Request = string:join([?NASA_IMAGES_HOST, Query], ""),
	io:format("Request: ~p~n", [Request]),
	case httpc:request(Request) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{ok, Body};
		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end.

parse_nasa_data_update_db(CelestialObject, MapOfNasaData) ->
		[RootDoc] = maps:keys(MapOfNasaData), % Expecting RootDoc =:= <<"collection">>
		RootMapItem = maps:get(RootDoc, MapOfNasaData), % Expecting [<<"href">>,<<"items">>,<<"links">>,<<"metadata">>, <<"version">>] 
		ItemsList = maps:get(<<"items">>, RootMapItem), %Expecting [item1, item2, ... , itemN]
		process_data_item(CelestialObject, ItemsList).

process_clean_collection_url(success, CelestialObject, DataList, CollectionUrl, ListTail) ->
	io:format("process_data_item:collection ~n~p~n", [CollectionUrl]),
	case fetch_collection_urls(CollectionUrl) of
		{ok, {ThumbImageUrl, LargImageUrl}} -> 
			% update the db record with this data
			% Should return a map with keys [<<"center">>,<<"date_created">>,<<"description">>, <<"description_508">>,<<"keywords">>,<<"media_type">>, <<"nasa_id">>,<<"secondary_creator">>,<<"title">>]
			DataMap = lists:nth(1, DataList), 	 
			DBItem = #celestial_object_table {
						key = CelestialObject,
						date = maps:get(<<"date_created">>, DataMap),
						title = maps:get(<<"title">>,DataMap),
						description = maps:get(<<"description">>, DataMap),
						url = ThumbImageUrl,
						hdurl = LargImageUrl
						},
			db_access:update_nasa_table(DBItem),
			process_data_item(CelestialObject, ListTail);
		{error, Error} ->
			io:format("Received error while fetching ~p~nError: ~p~n", [CollectionUrl, Error]),
			process_data_item(CelestialObject, ListTail)			
	end.

process_data_item(CelestialObject, [MapOfItems|T]) ->
	% fetch the data list. Should be a list of one item
	DataList = maps:get(<<"data">>, MapOfItems), % Get the data list
	CollectionUrl = maps:get(<<"href">>, MapOfItems), % Get the url to the asset that contains the URLs of jpg images
	IsVideo = binary:match(CollectionUrl, <<"video">>),
	if
		is_tuple(IsVideo) ->
			{error, <<"invalid URL: CollectionUrl">>};
		true ->
			process_clean_collection_url(success, CelestialObject, DataList, CollectionUrl, T)
	end;

process_data_item(_CelestialObject, []) -> ok.

fetch_collection_urls(Url) -> 
	case httpc:request(Url) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			JsonData = jiffy:decode(Body, []),
			ThumbImage = extract_image_url(start, <<"thumb">>, JsonData),		
			LargeImage = extract_image_url(start, <<"orig">>, JsonData),		
			Result = {ThumbImage, LargeImage},
			{ok, Result};
		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end.

extract_image_url(start, Target, JsonListData) ->
	extract_image_url(loop, Target, JsonListData);

extract_image_url(loop, Target, [Item|T]) ->
	NormalizedUrl = string:casefold(Item),
	NormalizedTarget = string:casefold(Target),
	MP4Result = binary:match(NormalizedUrl, <<".mp4">>), % Does the list contain vidio files?
	case MP4Result =:= nomatch of % No video files in the list, proceed with standard search
		true ->
			SearchResult = binary:match(NormalizedUrl, NormalizedTarget),
			if 
				SearchResult =/= nomatch ->
					extract_image_url(found, Item, []);
				true -> extract_image_url(loop, Target, T)
			end;
		false -> extract_image_url(loop, Target, T) % The list contains a video file. Skip this item
	end;


extract_image_url(found, Item, []) -> 
	io:format("Found: ~p~n", [Item]),
	Item;
extract_image_url(loop, _Target, []) -> error.