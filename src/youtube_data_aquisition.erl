% @Author: Oleg Zilberman
% @Date:   2023-02-23 17:50:53
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-21 10:48:08
-module(youtube_data_aquisition).
-export([fetch_data/3, fetch_single_channel/2, fetch_single_video/2]).
-include("include/server_config_item.hrl").
-include("include/macro_definitions.hrl").

%%%%TODO: fetch_sing_video() is not currently used. This function is the begining of a new feature.
%%%%TODO: A client should be able to have a collection of videos from different channels. For now
%%%% this is a massive change that needs to wait
fetch_single_video(ClientID, ChannelLink) ->
	{ok, YoutubeKey} = db_access:get_client_youtube_key(ClientID),
	fetch_video(ClientID, YoutubeKey, ChannelLink).

fetch_single_channel(ClientID, ChannelID) ->
	{ok, YoutubeKey} = db_access:get_client_youtube_key(ClientID),
	fetch_single_page([], #{}, [{YoutubeKey, ChannelID}], "10").

fetch_data(production, ClientProfile, Date) ->
	fetch_channel_data(Date, #{}, ClientProfile, ?YOUTUBE_MAXRESULTS); 

fetch_data(periodic, ClientProfile, Date) ->
	fetch_channel_data(Date, #{}, ClientProfile, ?YOUTUBE_MAXRESULTS).


fetch_video(ClientID, YoutubeKey, ChannelLink) ->
	Parts = string:split(ChannelLink, "/", all),
	if 
	    length(Parts) < 4 ->
	        {error, Message} = utils:format_error(?SERVER_ERROR_INVALID_LINK, video_link_wrong_format);
	    true ->
	       Target = lists:nth(4, Parts),
	       VideoParts = string:split(Target, "?", all),
	       [VideoID, _] = VideoParts,
	       Request = video_link_query(YoutubeKey, VideoID),
		    Options = [{ssl, [{verify, verify_none}]}],
			case httpc:request(get, {Request, []}, Options, []) of
				{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
					{ok, FirstPageMap} = utils:update_database(video_link, Body);
				{ok,{_,_,ErrorMessage}} ->
					io:format("fetch_channel_data failed: ~p~n", [ErrorMessage]),
					{error, ErrorMessage};
				Other ->
					{error, Other}
			end
	end.

fetch_single_page(Date, MasterMap, [Head|Tail], MaxResults) ->
	{YoutubeKey, ChannelID} = Head,
	Request = first_page_query(YoutubeKey, ChannelID, Date, MaxResults),
    Options = [{ssl, [{verify, verify_none}]}],
	case httpc:request(get, {Request, []}, Options, []) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{ok, FirstPageMap} = utils:update_database(youtube, Body),							% Commit data from the first fetch to the database
			PageMap = #{<<"first_page">> => FirstPageMap},										% First page does not have a token identifier
			case fetch_next_page(YoutubeKey, ChannelID, Date, [], MaxResults, PageMap) of
				{ok, RemainderChannelMap} ->
					CompleteChannelMap = maps:merge(PageMap, RemainderChannelMap),						% PageMap contains data for first_page. Merge it with the rest of the pages to get a map of all pages for this channel
					SectionMap = maps:put(ChannelID, CompleteChannelMap, #{}),							% Insert the map of all pages into a new map with the channel id as the key
					NewMaster = maps:merge(SectionMap, MasterMap),
					fetch_channel_data(Date, NewMaster, Tail, ?YOUTUBE_MAXRESULTS);
				{error, _ErrorMessage} ->
					fetch_channel_data(Date, MasterMap, [], ?YOUTUBE_MAXRESULTS)
			end;

		{ok,{_,_,ErrorMessage}} ->
			io:format("fetch_channel_data failed: ~p~n", [ErrorMessage]),
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end.

fetch_channel_data(_Date, NewMaster, [], ?YOUTUBE_MAXRESULTS) ->
	FinalMap = #{?TOP_KEY => NewMaster},
	FinalPackage = utils:reformat_channel_data(FinalMap),
	Json = jiffy:encode(#{?TOP_KEY =>FinalPackage}),
	{ok, Json};

fetch_channel_data(Date, MasterMap, [Head|Tail], MaxResults) ->
	{YoutubeKey, ChannelID} = Head,
	Request = first_page_query(YoutubeKey, ChannelID, Date, MaxResults),
    Options = [{ssl, [{verify, verify_none}]}],
	case httpc:request(get, {Request, []}, Options, []) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{ok, FirstPageMap} = utils:update_database(youtube, Body),							% Commit data from the first fetch to the database
			PageMap = #{<<"first_page">> => FirstPageMap},										% First page does not have a token identifier
			PageToken = get_next_page_token(FirstPageMap),										% Extract the next page token
			case fetch_next_page(YoutubeKey, ChannelID, Date, PageToken, MaxResults, PageMap) of
				{ok, RemainderChannelMap} ->
					CompleteChannelMap = maps:merge(PageMap, RemainderChannelMap),						% PageMap contains data for first_page. Merge it with the rest of the pages to get a map of all pages for this channel
					SectionMap = maps:put(ChannelID, CompleteChannelMap, #{}),							% Insert the map of all pages into a new map with the channel id as the key
					NewMaster = maps:merge(SectionMap, MasterMap),
					fetch_channel_data(Date, NewMaster, Tail, ?YOUTUBE_MAXRESULTS);
				{error, ErrorMessage} ->
					utils:log_message([{"Youtube error: ", ErrorMessage}]),
					fetch_channel_data(Date, MasterMap, [], ?YOUTUBE_MAXRESULTS)
			end;

		{ok,{_,_,ErrorMessage}} ->
			utils:log_message([{"fetch_channel_data failed:", ErrorMessage}]),
			{error, ErrorMessage};
		{error,enoent} ->
			utils:log_message([{"fetch_channel_data failed:", enoent}]),
			{error, enoent};

		Other ->
			utils:log_message([{"Other:", Other}]),
			{error, Other}
	end.

fetch_next_page(YoutubeKey, ChannelID, Date, [PageToken], MaxResults, Acc) ->
	Request = next_page_query(YoutubeKey, ChannelID, Date, PageToken, MaxResults),
    Options = [{ssl, [{verify, verify_none}]}],
	case httpc:request(get, {Request, []}, Options, []) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			{ok, Map} = utils:update_database(youtube, Body),
			Token = get_next_page_token(Map),
			fetch_next_page(YoutubeKey, ChannelID, Date, Token, MaxResults, maps:put(PageToken, Map, Acc)); 
			
		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end;
fetch_next_page(_ClientKey, _ChannelID, _Date, [], _MaxResults, Acc) -> {ok, Acc}.

get_next_page_token(Map) ->
	Keys = maps:keys(Map),
	Found = lists:any(fun(Element) -> Element =:= ?YOUTUBE_NEXTPAGE end, Keys),
	case Found of
		true ->
			[maps:get(?YOUTUBE_NEXTPAGE, Map)];
		false ->
			[]
	end.

video_link_query(YoutbeApiKey, VideoID) ->
	Query = uri_string:compose_query([{"key", YoutbeApiKey}, 
									  {"id", VideoID},
									  {"part", snippet}
									  ]),
	string:join([?YOUTBE_VIDEO_HOST, Query], "").

first_page_query(YoutbeApiKey, ChannelID, [Date], MaxResults) ->
	Query = uri_string:compose_query([{"key", YoutbeApiKey}, 
									  {"channelId", ChannelID},
									  {"part", "snippet,id"},
									  {"order", "date"},
									  {"publishedAfter", Date},
									  {"maxResults", MaxResults}
									  ]),
	string:join([?YOUTUBE_SEARCH_HOST, Query], "");

first_page_query(YoutbeApiKey, ChannelID, [], MaxResults) ->
	Query = uri_string:compose_query([{"key", YoutbeApiKey}, 
									  {"channelId", ChannelID},
									  {"part", "snippet,id"},
									  {"order", "date"},
  									  {"publishedAfter", ?FIRST_PUBLISH_DATE},
									  {"maxResults", MaxResults}
									  ]),
	string:join([?YOUTUBE_SEARCH_HOST, Query], "").

next_page_query(YoutbeApiKey, ChannelID, [Date], PageToken, MaxResults) ->
	Query = uri_string:compose_query([{"key", YoutbeApiKey}, 
									  {"channelId", ChannelID},
									  {"part", "snippet,id"},
									  {"order", "date"},
  									  {"publishedAfter", Date},
									  {"maxResults", MaxResults},
									  {"pageToken", PageToken}
									  ]),
	string:join([?YOUTUBE_SEARCH_HOST, Query], "");

next_page_query(YoutbeApiKey, ChannelID, [], PageToken, MaxResults) ->
	Query = uri_string:compose_query([{"key", YoutbeApiKey}, 
									  {"channelId", ChannelID},
									  {"part", "snippet,id"},
									  {"order", "date"},
									  {"maxResults", MaxResults},
  									  {"publishedAfter", ?FIRST_PUBLISH_DATE},
									  {"pageToken", PageToken}
									  ]),
	string:join([?YOUTUBE_SEARCH_HOST, Query], "").
