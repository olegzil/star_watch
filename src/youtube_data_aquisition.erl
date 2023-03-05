% @Author: Oleg Zilberman
% @Date:   2023-02-23 17:50:53
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-04 17:30:21
-module(youtube_data_aquisition).
-export([fetch_data/3]).
-include("include/macro_definitions.hrl").

fetch_data(production, ChannelId, Date) ->
	fetch_channel_data(Date, [ChannelId], #{}); 

fetch_data(periodic, ChannelIDs, Date) ->
	fetch_channel_data(Date, ChannelIDs, #{}).

fetch_channel_data(_Date, [], Acc) -> 
	{ok, Acc};

fetch_channel_data(Date, [ChannelID | Tail], MasterMap) ->
	Request = first_page_query(ChannelID, Date),

	case httpc:request(Request) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{ok, FirstPageMap} = utils:update_database(youtube, Body),							% Commit data from the first fetch to the database
			PageMap = #{<<"first_page">> => FirstPageMap},										% First page does not have a token identifier
			PageToken = get_next_page_token(FirstPageMap),										% Extract the next page token
			{ok, RemainderChannelMap} = fetch_next_page(ChannelID, Date, PageToken, PageMap), 	% Atempt to fetch data for the next page, or return
			CompleteChannelMap = maps:merge(PageMap, RemainderChannelMap),						% PageMap contains data for first_page. Merge it with the rest of the pages to get a map of all pages for this channel
			SectionMap = maps:put(ChannelID, CompleteChannelMap, #{}),							% Insert the map of all pages into a new map with the channel id as the key
			NewMaster = maps:merge(SectionMap, MasterMap),
			{ok, CompositeMap} = fetch_channel_data(Date, Tail, NewMaster),						% Repeat the process with a map that already contains a channel and all its data
			FinalMap = #{?TOP_KEY => CompositeMap},
			FinalPackage = utils:reformat_channel_data(FinalMap),
			Json = jiffy:encode(#{?TOP_KEY =>FinalPackage}),
			{ok, Json};

		{ok,{_,_,ErrorMessage}} ->
			io:format("fetch_channel_data failed: ~p~n", [ErrorMessage]),
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end.

fetch_next_page(ChannelID, Date, [PageToken], Acc) ->
	Request = next_page_query(ChannelID, Date, PageToken),
	case httpc:request(Request) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{ok, Map} = utils:update_database(youtube, Body),
			Token = get_next_page_token(Map),
			fetch_next_page(ChannelID, Date, Token, maps:put(PageToken, Map, Acc)); %%% TODO: last arg should be a map
			
		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end;
fetch_next_page(_ChannelID, _Date, [], Acc) -> {ok, Acc}.

get_next_page_token(Map) ->
	Keys = maps:keys(Map),
	Found = lists:any(fun(Element) -> Element =:= ?YOUTUBE_NEXTPAGE end, Keys),
	case Found of
		true ->
			[maps:get(?YOUTUBE_NEXTPAGE, Map)];
		false ->
			[]
	end.


first_page_query(ChannelID, [Date]) ->
	{ok, YoutbeApiKey} = utils:fetch_youtube_api_key(),
	Query = uri_string:compose_query([{"key", YoutbeApiKey}, 
									  {"channelId", ChannelID},
									  {"part", "snippet,id"},
									  {"order", "date"},
									  {"publishedAfter", Date},
									  {"maxResults", ?YOUTUBE_MAXRESULTS}
									  ]),
	string:join([?YOUTUBE_HOST, Query], "");

first_page_query(ChannelID, []) ->
	{ok, YoutbeApiKey} = utils:fetch_youtube_api_key(),
	Query = uri_string:compose_query([{"key", YoutbeApiKey}, 
									  {"channelId", ChannelID},
									  {"part", "snippet,id"},
									  {"order", "date"},
  									  {"publishedAfter", ?FIRST_PUBLISH_DATE},
									  {"maxResults", ?YOUTUBE_MAXRESULTS}
									  ]),
	string:join([?YOUTUBE_HOST, Query], "").

next_page_query(ChannelID, [Date], PageToken) ->
	{ok, YoutbeApiKey} = utils:fetch_youtube_api_key(),
	Query = uri_string:compose_query([{"key", YoutbeApiKey}, 
									  {"channelId", ChannelID},
									  {"part", "snippet,id"},
									  {"order", "date"},
  									  {"publishedAfter", Date},
									  {"maxResults", ?YOUTUBE_MAXRESULTS},
									  {"pageToken", PageToken}
									  ]),
	string:join([?YOUTUBE_HOST, Query], "");

next_page_query(ChannelID, [], PageToken) ->
	{ok, YoutbeApiKey} = utils:fetch_youtube_api_key(),
	Query = uri_string:compose_query([{"key", YoutbeApiKey}, 
									  {"channelId", ChannelID},
									  {"part", "snippet,id"},
									  {"order", "date"},
									  {"maxResults", ?YOUTUBE_MAXRESULTS},
  									  {"publishedAfter", ?FIRST_PUBLISH_DATE},
									  {"pageToken", PageToken}
									  ]),
	string:join([?YOUTUBE_HOST, Query], "").
