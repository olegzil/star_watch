% @Author: Oleg Zilberman
% @Date:   2023-01-12 10:11:59
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-25 16:37:12
-module(nasa_rest_access).
-include("include/macro_definitions.hrl").
-include("include/celestial_object_table.hrl").
-export([fetch_root_page/4, parse_nasa_data_update_db/2]).

%%% This function makes an http call to retrive the json for the celestial object and page specified
fetch_root_page(Subject, GregorianStartDays, GregorianEndDays, Page) ->
    io:format("fetch_root_page:Page = ~p~n", [Page]),

	CelestialObject = string:casefold(Subject),
	{StartYear, _StartMonth, _StartDay} = calendar:gregorian_days_to_date(GregorianStartDays), 
	{EndYear, _EndMonth, _EndDay} = calendar:gregorian_days_to_date(GregorianEndDays),
	Past = binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p",[StartYear]))),
	Future = binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p",[EndYear]))),
	TargetTuple = lists:keyfind(list_to_atom(CelestialObject), 1, ?CELESTIAL_OBJECTS),
	SearchTarget = atom_to_list(element(1, TargetTuple)),
	ActionPair = element(2, TargetTuple),
	Verb = atom_to_list(element(1, ActionPair)), 
	Object = list_to_binary(element(2, ActionPair)),

	Query = uri_string:compose_query([{"q", SearchTarget},
									  {Verb, Object},	
									  {"year_start", Past}, 
									  {"year_end", Future},
									  {"page", integer_to_list(Page)}
									  ]),
	Request = string:join([?NASA_IMAGES_HOST, Query], ""),
	io:format("Request ~p~n", [Request]),
	case httpc:request(Request) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{ok, Body};
		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end.

parse_nasa_data_update_db(CelestialObject, MapOfNASAData) ->
		[RootDoc] = maps:keys(MapOfNASAData), % Expecting RootDoc =:= <<"collection">>
		RootMapItem = maps:get(RootDoc, MapOfNASAData), % Expecting [<<"href">>,<<"items">>,<<"links">>,<<"metadata">>, <<"version">>] 
		ItemsList = maps:get(<<"items">>, RootMapItem), %Expecting [item1, item2, ... , itemN]
		process_data_item(CelestialObject, ItemsList).

process_clean_collection_url(CelestialObject, DataList, CollectionUrl, ListTail) ->
	if
		length(ListTail) =/= 0 ->
			case fetch_collection_urls(CollectionUrl) of
				{ok, {ThumbImageUrl, LargImageUrl}} -> 
					% update the db record with this data
					% Should return a map with keys [<<"center">>,<<"date_created">>,<<"description">>, <<"description_508">>,<<"keywords">>,<<"media_type">>, <<"nasa_id">>,<<"secondary_creator">>,<<"title">>]
					DataMap = lists:nth(1, DataList), 	 
					DBItem = #celestial_object_table {
								key = CelestialObject,
								date = 			maps:get(<<"date_created">>, DataMap, <<"notfound">>),
								title = 		maps:get(<<"title">>, 		 DataMap, <<"notfound">>),
								description = 	maps:get(<<"description">>,  DataMap, <<"notfound">>),
								center = 		maps:get(<<"center">>, 		 DataMap, <<"notfound">>),
								nasa_id = 		maps:get(<<"nasa_id">>, 	 DataMap, <<"notfound">>),
								keywords = 		maps:get(<<"keywords">>, 	 DataMap, <<"notfound">>),
								url = ThumbImageUrl,
								hdurl = LargImageUrl
								},
					db_access:update_nasa_table(DBItem),
					process_data_item(CelestialObject, ListTail);
				{error, Error} ->
					io:format("Received error while fetching ~p~nError: ~p~n", [CollectionUrl, Error]),
					process_data_item(CelestialObject, ListTail)			
			end;
			true -> ok
			
	end.

process_data_item(CelestialObject, [NextItem|T]) ->
	% fetch the data list. Should be a list of one item
	DataMap = maps:get(<<"data">>, NextItem), % Get the data list
	CollectionUrl = maps:get(<<"href">>, NextItem), % Get the url to the asset that contains the URLs of jpg images
	IsVideo = binary:match(CollectionUrl, <<"video">>),
	IsAudio = binary:match(CollectionUrl, <<"audio">>),
	if
		is_tuple(IsVideo) orelse is_tuple(IsAudio) ->
			process_data_item(CelestialObject, T);
		true ->
			Match = apply_filter(DataMap, ?EXCLUTION_LIST),
			case Match of
			 	true ->
					process_data_item(CelestialObject, T);
				false ->
					process_clean_collection_url(CelestialObject, DataMap, CollectionUrl, T)
			end
	end;

process_data_item(_CelestialObject, []) -> 
	io:format("********** process_data_item ZERO LENGTH LIST FOUND~n"),
	ok.

%%% TODO:CODE return a tuple {notfound, Key, Needles, Heystack} if the offending token has been found.
apply_filter(DataList, [H|T]) ->
	DataMap = lists:nth(1, DataList), 	 
	Key = atom_to_binary(element(1, H)), 
	Needles = utils:generate_coparable_list(element(2, H)), %% Retrieve the needles to look for in the heystack and make sure they're lower case
	Heystack = maps:get(Key, DataMap, notfound),
	case Heystack of
		notfound -> 
			apply_filter(DataList, T);
		_ -> 
			Found = filter_based_on_key(Key, Needles, Heystack),
			if
				Found =:= false ->
					apply_filter(DataList, T);
				true ->
					true
			end
	end;

apply_filter(_DataList, []) -> false.

filter_based_on_key(Key, Needles, Target) ->
	case Key of
		<<"center">> ->
			Result = lists:member(string:casefold(Target), Needles),
			Result;
		<<"description">> ->
			NormalizedStr = binary_to_list(Target),
			Heystack = [Char || Char <- NormalizedStr, Char > 31, Char < 127], %% only ascii
			Result = utils:find_token_in_string(Heystack, Needles),
			Result;
		<<"keywords">> ->
			NormalizedList = utils:generate_coparable_list(Target),
			Result = [] =/= [LHS || LHS <- NormalizedList, RHS <- Needles, LHS =:= RHS],
			Result;
		<<"nasa_id">> ->
			lists:member(Target, Needles);
		<<"title">> ->
			String = string:casefold(Target),
			Result = utils:find_token_in_string(String, Needles),
			Result;
		_ ->
			io:format("Invalid search: ~p~n", [Key]),
			false
	end.

fetch_collection_urls(Url) -> 
	case httpc:request(Url) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			JsonData = jiffy:decode(Body, []),
			ThumbImage = extract_image_url(start, <<"thumb">>, JsonData),		
			LargeImage = extract_image_url(start, <<"orig">>, JsonData),		
			Heystack = {ThumbImage, LargeImage},
			{ok, Heystack};
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
	MP4Heystack = binary:match(NormalizedUrl, <<".mp4">>), % Does the list contain vidio files?
	case MP4Heystack =:= nomatch of % No video files in the list, proceed with standard search
		true ->
			SearchHeystack = binary:match(NormalizedUrl, NormalizedTarget),
			if 
				SearchHeystack =/= nomatch ->
					extract_image_url(found, Item, []);
				true -> extract_image_url(loop, Target, T)
			end;
		false -> extract_image_url(loop, Target, T) % The list contains a video file. Skip this item
	end;


extract_image_url(found, Item, []) -> 
	Item;
extract_image_url(loop, _Target, []) -> error.