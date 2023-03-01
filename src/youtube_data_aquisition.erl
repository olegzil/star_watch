% @Author: Oleg Zilberman
% @Date:   2023-02-23 17:50:53
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-02-28 16:52:36
-module(youtube_data_aquisition).
-export([fetch_data/2]).
-include("include/macro_definitions.hrl").

fetch_data(production, ChannelId) ->
	fetch_channel_data([ChannelId], #{}); 

fetch_data(periodic, ChannelIDs) ->
	fetch_channel_data(ChannelIDs, #{}).

fetch_channel_data([ChannelID | Tail], MasterMap) ->
	Section = ChannelID,
	Request = first_page_query(ChannelID),

	case httpc:request(Request) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{ok, FirstPageMap} = utils:update_database(youtube, Body),					% Commit data from the first fetch to the database
			PageMap = #{<<"first_page">> => FirstPageMap},								% First page does not have a token identifier
			PageToken = get_next_page_token(FirstPageMap),								% Extract the next page token
			{ok, RemainderChannelMap} = fetch_next_page(ChannelID, PageToken, PageMap), 	% Atempt to fetch data for the next page, or return
			CompleteChannelMap = maps:merge(PageMap, RemainderChannelMap),						% PageMap contains data for first_page. Merge it with the rest of the pages to get a map of all pages for this channel
			TempMap = maps:put(Section, CompleteChannelMap, #{}),						% Insert the map of all pages into a new map with the channel id as the key
			NewMaster = maps:merge(TempMap, MasterMap),
			{ok, MapData} = fetch_channel_data(Tail, NewMaster),						% Repeat the process with a map that already contains a channel and all its data
			{ok, jiffy:encode(MapData)};

		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end;

fetch_channel_data([], Acc) -> {ok, Acc}.

fetch_next_page(ChannelID, [PageToken], Acc) ->
	Request = next_page_query(ChannelID, PageToken),
	case httpc:request(Request) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{ok, Map} = utils:update_database(youtube, Body),
			Token = get_next_page_token(Map),
			fetch_next_page(ChannelID, Token, maps:put(PageToken, Map, Acc)); %%% TODO: last arg should be a map
			
		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end;
fetch_next_page(_ChannelID, [], Acc) -> {ok, Acc}.

get_next_page_token(Map) ->
	Keys = maps:keys(Map),
	Found = lists:any(fun(Element) -> Element =:= ?YOUTUBE_NEXTPAGE end, Keys),
	case Found of
		true ->
			[maps:get(?YOUTUBE_NEXTPAGE, Map)];
		false ->
			io:format("Next page token not found~n"),
			[]
	end.


first_page_query(ChannelID) ->
	Query = uri_string:compose_query([{"key", ?YOUTUBE_API_KEY}, 
									  {"channelId", ChannelID},
									  {"part", "snippet,id"},
									  {"order", "date"},
									  {"maxResults", ?YOUTUBE_MAXRESULTS}
									  ]),
	string:join([?YOUTUBE_HOST, Query], "").

next_page_query(ChannelID, PageToken) ->
	Query = uri_string:compose_query([{"key", ?YOUTUBE_API_KEY}, 
									  {"channelId", ChannelID},
									  {"part", "snippet,id"},
									  {"order", "date"},
									  {"maxResults", ?YOUTUBE_MAXRESULTS},
									  {"pageToken", PageToken}
									  ]),
	string:join([?YOUTUBE_HOST, Query], "").
