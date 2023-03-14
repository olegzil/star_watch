% @Author: Oleg Zilberman
% @Date:   2023-03-06 15:30:12
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-13 17:17:01
-module(server_config_processor).
-include("include/server_config_item.hrl").
-export([fetch_client_config_data/1, 
		 fetch_list_of_channel_ids_and_youtube_keys/0,
		 fetch_profile_map/0,
		 fetch_list_of_client_ids_and_channel_ids/0,
		 fetch_client_ids_and_names/0,
		 update_config_record/2,
		 delete_record/2]).

fetch_client_config_data(ClientKey) ->
	{ok, List} = file:consult("server_config.cfg"),
	Result = fetch_data_for_key(ClientKey, List),
	case Result of
		error ->
			error;
		Value ->
			{Name, [{_, YoutubeKey}, {_, ChannelId} ]} = Value,
			#server_config_item{
				name = Name,
				client_id = ClientKey, 
				youtubekey = YoutubeKey,
				channel_id = ChannelId
			}
	end.

fetch_profile_map() ->
	{ok, List} = file:consult("server_config.cfg"),
	fetch_all_items(List, #{}).	

fetch_all_items([], Acc) -> Acc;

fetch_all_items([Client|Tail], Acc) ->
	%%% Client = #{<<"cad00c93-b012-49f9-97f9-35e69ae083a0">> => {sonomaashram, [{youtubekey, <<"AIzaSyAHvRD_wu1xR8D_fmJwkiPO0jqw_rnhvHQ">>}, {channel_id, <<"UCQfZkf3-Y2RwzdRFWXYsdaQ">>} ]}}.
	[Key] = maps:keys(Client),
	{ok, Value} = maps:find(Key, Client),
	{Name,[{_, YoutubeID},{_,ChannelID}]} = Value,
	NewMap = maps:put(Key, {Name, YoutubeID, ChannelID}, #{}),
	fetch_all_items(Tail, maps:merge(Acc, NewMap)).

fetch_list_of_channel_ids_and_youtube_keys() ->
	{ok, List} = file:consult("server_config.cfg"),
	fetch_client_and_youtube_ids(List, []).

fetch_client_and_youtube_ids([], Acc) -> Acc;

fetch_client_and_youtube_ids([Client|Tail], Acc) ->
	%%% Client = #{<<"cad00c93-b012-49f9-97f9-35e69ae083a0">> => {sonomaashram, [{youtubekey, <<"AIzaSyAHvRD_wu1xR8D_fmJwkiPO0jqw_rnhvHQ">>}, {channel_id, <<"UCQfZkf3-Y2RwzdRFWXYsdaQ">>} ]}}.
	[Key] = maps:keys(Client),
	{ok, Value} = maps:find(Key, Client),
	{_, [{_, YoutubeKey}, {_, ChannelId} ]} = Value,
	NewList = lists:merge(Acc, [{YoutubeKey, ChannelId}]),
	fetch_client_and_youtube_ids(Tail, NewList).
	

fetch_data_for_key(ClientKey, [Map|Tail]) ->
	case maps:find(ClientKey, Map) of
		{ok, Value} ->
			Value;
		error -> 
			fetch_data_for_key(ClientKey, Tail)
	end;

fetch_data_for_key(_ClientKey, []) ->
	error.

fetch_list_of_client_channel_tuples([], Acc) -> Acc;

fetch_list_of_client_channel_tuples([Client|Tail], Acc) ->
	% #{Client => {sonomaashram, [{youtubekey, <<"AIzaSyAHvRD_wu1xR8D_fmJwkiPO0jqw_rnhvHQ">>}, {channel_id, <<"UCQfZkf3-Y2RwzdRFWXYsdaQ">>} ]}}.
	[ClientID] = maps:keys(Client),
	{ok, Value} = maps:find(ClientID, Client),
	{_, [{_, _}, {_, ChannelId} ]} = Value,
	NewList = lists:merge(Acc, [{ClientID, ChannelId}]),
	fetch_list_of_client_channel_tuples(Tail, NewList).


fetch_list_of_client_ids_and_channel_ids() ->
	{ok, List} = file:consult("server_config.cfg"),
	fetch_list_of_client_channel_tuples(List, []).

fetch_client_ids_and_names() ->
	{ok, List} = file:consult("server_config.cfg"),
	fetch_ids_and_names(List, []).	

fetch_ids_and_names([], Acc) -> Acc;
fetch_ids_and_names([Client|Tail], Acc) ->
	% #{Client => {sonomaashram, [{youtubekey, <<"AIzaSyAHvRD_wu1xR8D_fmJwkiPO0jqw_rnhvHQ">>}, {channel_id, <<"UCQfZkf3-Y2RwzdRFWXYsdaQ">>} ]}}.
	[ClientID] = maps:keys(Client),
	{ok, Value} = maps:find(ClientID, Client),
	{Name, [{_, _}, {_, ChannelId} ]} = Value,
	NewList = lists:merge(Acc, [#{<<"name">> => atom_to_binary(Name), <<"client">> => ClientID, <<"channel_id">> => ChannelId}]),
	fetch_ids_and_names(Tail, NewList).

delete_record(File, ClientID) ->
	{ok, List} = file:consult(File),
	file:delete(File),
	delete_record_find_key(File, ClientID, List).

delete_record_find_key(File, ClientID, List) ->
	FilterPredicate = fun(Map) ->
						case maps:find(ClientID, Map) of
							{ok, _Value} ->
								false;
							error ->
								true
						end
					  end,
	NewList = lists:filter(FilterPredicate, List),
	delete_record_update_file(File, NewList).

delete_record_update_file(_File, []) -> ok;
delete_record_update_file(File, [Map|Tail]) ->
	[Key] = maps:keys(Map),
	update_config_file(File, Key, Map),
	delete_record_update_file(File, Tail).


update_config_record(File, NewRecord) ->
	[Key] = maps:keys(NewRecord),
    #{Key := {Name, [{youtubekey, YoutubeKey}, {channel_id, ChannelID} ] }} = NewRecord,
    ListResult = io_lib:format("#{~p => {~p, [{youtubekey, ~p}, {channel_id, ~p} ] }}~n", [Key, Name, YoutubeKey, ChannelID]),
    ConfigRecord = list_to_binary(ListResult),

	case fetch_client_config_data(Key) of
		error -> %% This key does not exist. Add it.
			file:write_file(File, ConfigRecord, [append]);
		_OldRecord ->
			Map = fetch_profile_map(),
			Record = {Name, YoutubeKey, ChannelID},
			NewMap = maps:put(Key, Record, Map),
			file:delete(File),
			write_map(File, NewMap, maps:keys(NewMap))
	end.
write_map(_File, _NewMap, []) -> ok;

write_map(File, NewMap, [Key|Tail]) ->
	update_config_file(File, Key, NewMap),
	write_map(File, NewMap, Tail).
	
update_config_file(File, Key, NewMap) ->
	case maps:find(Key, NewMap) of
		{ok,{Name,YoutubeKey, ChannelID}} ->
		    ListResult = io_lib:format("#{~p => {~p, [{youtubekey, ~p}, {channel_id, ~p} ] }}.~n", [Key, Name, YoutubeKey, ChannelID]),
		    ConfigRecord = list_to_binary(ListResult),
		    io:format("ConfigRecord ~p~n",[ConfigRecord]),
			file:write_file(File, ConfigRecord,[append]);
		{ok,{Name,[{youtubekey,YoutubeKey}, {channel_id,ChannelID}]}} ->
		    ListResult = io_lib:format("#{~p => {~p, [{youtubekey, ~p}, {channel_id, ~p} ] }}.~n", [Key, Name, YoutubeKey, ChannelID]),
		    ConfigRecord = list_to_binary(ListResult),
		    io:format("ConfigRecord ~p~n",[ConfigRecord]),
			file:write_file(File, ConfigRecord,[append])
	end.
