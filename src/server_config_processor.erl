% @Author: Oleg Zilberman
% @Date:   2023-03-06 15:30:12
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-17 22:01:24
-module(server_config_processor).
-include("include/server_config_item.hrl").
-include("include/error_responses.hrl").
% -export([fetch_client_config_data/2, 
% 		 fetch_list_of_channel_ids_and_youtube_keys/1,
% 		 fetch_profile_map/1,
% 		 fetch_list_of_client_ids_and_channel_ids/0,
% 		 fetch_client_ids_and_names/0,
% 		 update_config_record/2,
% 		 delete_record/2,
% 		 get_default_youtube_key/1, 
% 		 fetch_list_of_channel_ids_and_youtube_keys_jsonified/1,
% 		 update_client_profiles/2]).
-compile(export_all).

parse_server_config_file(File) ->
	{ok, ServerConfig} = file:consult(File),
	[ControlBlock, ClientProfiles] = ServerConfig,
	#{
		profiles => ClientProfiles,
		control_block => ControlBlock
	}.

get_profiles_list(File) ->
	MasterMap = parse_server_config_file(File),
	Map = maps:get(profiles, MasterMap),
	maps:get(client_profiles, Map).

get_server_control_block(File) ->
	MasterMap = parse_server_config_file(File),
	maps:get(control_block, MasterMap).

get_default_youtube_key(File) ->
	ControlBlock = get_server_control_block(File),
	Map = maps:get(server_control_block, ControlBlock),
	maps:get(default_youtubekey, Map).

update_client_profiles(File, List) ->
	MasterMap =parse_server_config_file(File),
	NewMaster = maps:put(profiles, List, MasterMap),
	write_server_config(File, NewMaster).

delete_record_update_file(_File, []) -> ok;
delete_record_update_file(File, [Map|Tail]) ->
	[Key] = maps:keys(Map),
	update_config_file(delete_record, File, Key, Map),
	delete_record_update_file(File, Tail).



fetch_client_config_data(File,ClientKey) ->
	Result = fetch_client_config_data_private(File, ClientKey),
	case Result of
		error ->
			Message = utils:jsonify_list_of_tuples([error, client_key], [{<<"client id not found">>, ClientKey}]),
			{error, Message};
		Value ->
			Message = utils:jsonify_list_of_tuples([client_key, name, youtubekey, channel_id], [{ClientKey, Value#server_config_item.name, Value#server_config_item.youtubekey, Value#server_config_item.channel_id}]),
			{ok, Message}
	end.

fetch_client_config_data_private(File, ClientKey) ->
	List = get_profiles_list(File),
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

fetch_profile_map(FileName) ->
	List = get_profiles_list(FileName),
	{ok, #{items => fetch_all_items(List, [])}}.

fetch_all_items([], Acc) -> Acc;
fetch_all_items([Client|Tail], Acc) ->
	%%% Client = #{<<"cad00c93-b012-49f9-97f9-35e69ae083a0">> => {sonomaashram, [{youtubekey, <<"AIzaSyAHvRD_wu1xR8D_fmJwkiPO0jqw_rnhvHQ">>}, {channel_id, <<"UCQfZkf3-Y2RwzdRFWXYsdaQ">>} ]}}.
	[Key] = maps:keys(Client),
	{ok, Value} = maps:find(Key, Client),
	{Name,[{_, YoutubeID},{_,ChannelID}]} = Value,
	MapList = [maps:put(client_id, Key, #{}), maps:put(name, Name, #{}), maps:put(youtube_id, YoutubeID, #{}), maps:put(channel_id, ChannelID, #{})],
	NewMap = merge_maps(MapList, #{}),
	fetch_all_items(Tail, lists:append(Acc, [NewMap])).

	merge_maps([], Acc) -> Acc;
	merge_maps([Head|Tail], Acc) ->
		merge_maps(Tail, maps:merge(Acc, Head)).

fetch_list_of_channel_ids_and_youtube_keys(FileName) ->
	List = get_profiles_list(FileName),
	#{ok => fetch_client_and_youtube_ids(List, [])}.


fetch_list_of_channel_ids_and_youtube_keys_jsonified(FileName) ->
	List = get_profiles_list(FileName),
	{TupleList} = fetch_client_and_youtube_ids(List, []),
	{ok, utils:jsonify_list_of_tuples([youtube_key, channel_id], TupleList)}.

fetch_client_and_youtube_ids([], Acc) -> {Acc};

fetch_client_and_youtube_ids([Client|Tail], Acc) ->
	%%% Client = #{<<"cad00c93-b012-49f9-97f9-35e69ae083a0">> => {sonomaashram, [{youtubekey, <<"AIzaSyAHvRD_wu1xR8D_fmJwkiPO0jqw_rnhvHQ">>}, {channel_id, <<"UCQfZkf3-Y2RwzdRFWXYsdaQ">>} ]}}.
	[Key] = maps:keys(Client),
	{ok, Value} = maps:find(Key, Client),
	{_, [{_, YoutubeKey}, {_, ChannelId} ]} = Value,
	UpdatedList = lists:merge(Acc, [{YoutubeKey, ChannelId}]),
	fetch_client_and_youtube_ids(Tail, UpdatedList).
	

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
	UpdatedList = lists:merge(Acc, [{ClientID, ChannelId}]),
	fetch_list_of_client_channel_tuples(Tail, UpdatedList).


fetch_list_of_client_ids_and_channel_ids() ->
	List = get_profiles_list("server_config.cfg"),
	TupleList = fetch_list_of_client_channel_tuples(List, []),
	io:format("TupleList: ~p~n", [TupleList]),
	{ok, #{items => utils:jsonify_list_of_tuples([client_id, channel_id], TupleList)}}.

fetch_client_ids_and_names() ->
	List = get_profiles_list("server_config.cfg"),
	{ok, #{directoryrecords => fetch_ids_and_names(List, [])}}.	

fetch_ids_and_names([], Acc) -> Acc;
fetch_ids_and_names([Client|Tail], Acc) ->
	% #{Client => {sonomaashram, [{youtubekey, <<"AIzaSyAHvRD_wu1xR8D_fmJwkiPO0jqw_rnhvHQ">>}, {channel_id, <<"UCQfZkf3-Y2RwzdRFWXYsdaQ">>} ]}}.
	[ClientID] = maps:keys(Client),
	{ok, Value} = maps:find(ClientID, Client),
	{Name, [{_, _}, {_, ChannelId} ]} = Value,
	UpdatedList = lists:merge(Acc, [#{<<"name">> => atom_to_binary(Name), <<"client">> => ClientID, <<"channel_id">> => ChannelId}]),
	fetch_ids_and_names(Tail, UpdatedList).

delete_record(File, ClientID) ->
	List = get_profiles_list("server_config.cfg"),
	delete_record_find_key(File, ClientID, List).

delete_record_find_key(File, ClientID, List) ->
	{UpdatedList, Found} = remove_clientID_from_list(ClientID, List),
	case Found of
		true ->
		    Item = fetch_client_config_data_private(File, ClientID),
			Message = utils:jsonify_list_of_tuples([success, client_id, name, youtubekey, channel_id], [{
																								<<"Record deleted">>,
																								Item#server_config_item.client_id, 
																								Item#server_config_item.name,
																								Item#server_config_item.youtubekey,
																								Item#server_config_item.channel_id
																							   }]),
			server_config_processor:update_client_profiles(File, UpdatedList),
			{ok, Message};
		false ->
			Message = utils:jsonify_list_of_tuples([error, client_key], [{<<"client id not found">>, ClientID}]),
			{error, Message}
	end.

remove_clientID_from_list(ClientID, List) ->
	OriginalSize = length(List),
	FilterPredicate = fun(Map) ->
						case maps:find(ClientID, Map) of
							{ok, _Value} ->
								false;
							error ->
								true
						end
					  end,
	UpdatedList = lists:filter(FilterPredicate, List),
	NewSize = length(UpdatedList),
	Found = (NewSize < OriginalSize),
	{UpdatedList, Found}.


fetch_profile_map_private() ->
	ServerControl = get_profiles_list("server_config.cfg"),
	List = maps:get(profiles, ServerControl),
	generate_config_list(List, []).

generate_config_list([], Acc) ->Acc;
generate_config_list([Map|Tail], Acc) ->
	[Value] = maps:values(Map),
	[Key] = maps:keys(Map),
	{Name, [{_, YoutubeKey}, {_, ChannelId} ]} = Value,
	ListItem = #server_config_item{
		name = Name,
		client_id = Key, 
		youtubekey = YoutubeKey,
		channel_id = ChannelId
	},
	generate_config_list(Tail, lists:append(Acc, [ListItem])).

write_server_config(File, Map) ->
	ServerControlBlockMap = get_server_control_block(File),
	ServerControlBlock = maps:get(server_control_block, ServerControlBlockMap),
    file:write_file(File, io_lib:format("#{~n ~cserver_control_block => #{default_youtubekey => ~p}~n}.~n", [9, maps:get(default_youtubekey, ServerControlBlock)])),
    ClientProfilesList = maps:get(profiles, Map),
    file:write_file(File, list_to_binary(io_lib:format(<<"#{ client_profiles =>~n[~n">>, [])), [append]),
    format_data_and_write_map(File, ClientProfilesList),
    file:write_file(File, list_to_binary(io_lib:format(<<"]~n}.~n">>, [])), [append]).

format_data_and_write_map(_Arg1, []) -> ok;
format_data_and_write_map(File, [Item|Tail]) ->
	[ClientID] = maps:keys(Item),
	Record = maps:values(Item),
	[{Name,[{_, YoutubeKey},{_,ChannelID}]}] = Record,
	case length(Tail) of
		0 -> %%% Do not terminate string with a ',''
		    ConfigRecord = list_to_binary(io_lib:format("#{~c~p => {~p, [{youtubekey, ~p}, {channel_id, ~p} ] }}~n", [9, ClientID, Name, YoutubeKey, ChannelID])),
			file:write_file(File, ConfigRecord, [append]);
		_ -> %%% Terminate string with a ','
		    ConfigRecord = list_to_binary(io_lib:format("#{~c~p => {~p, [{youtubekey, ~p}, {channel_id, ~p} ] }},~n", [9, ClientID, Name, YoutubeKey, ChannelID])),
			file:write_file(File, ConfigRecord, [append])
	end,
	format_data_and_write_map(File, Tail).



update_config_record(File, NewRecord) ->
	OldMap = parse_server_config_file(File),
    Profiles = maps:get(profiles, OldMap),
    ProfileList = maps:get(client_profiles, Profiles),
    [ClientID]  = maps:keys(NewRecord),
    {UpdatedList, Found} = remove_clientID_from_list(ClientID, ProfileList),
	NewList = lists:append(UpdatedList, [NewRecord]),
	write_server_config(File, maps:put(profiles, NewList, OldMap)),

	case Found of
		true ->
			{ok, compose_success_message(<<"Existing record updated">>, NewRecord)};
		false ->
			{ok, compose_success_message(<<"New entry added">>, NewRecord)}
	end.

write_map(_File, []) -> ok;
write_map(File, [Record|Tail]) ->
	update_config_file(update_record, File, Record, []),
	write_map(File, Tail).
	

update_config_file(update_record, File, Record, _Arg2) ->
	ListResult = io_lib:format("#{~p => {~p, [{youtubekey, ~p}, {channel_id, ~p} ] }}.~n", 
								[Record#server_config_item.client_id, 
								Record#server_config_item.name, 
								Record#server_config_item.youtubekey, 
								Record#server_config_item.channel_id]),
	ConfigRecord = list_to_binary(ListResult),
	file:write_file(File, ConfigRecord,[append]);

update_config_file(delete_record, File, Key, NewMap) ->
	case maps:find(Key, NewMap) of
		{ok,{Name,YoutubeKey, ChannelID}} ->
		    ListResult = io_lib:format("#{~p => {~p, [{youtubekey, ~p}, {channel_id, ~p} ] }}.~n", [Key, Name, YoutubeKey, ChannelID]),
		    ConfigRecord = list_to_binary(ListResult),
			file:write_file(File, ConfigRecord,[append]);
		{ok,{Name,[{youtubekey,YoutubeKey}, {channel_id,ChannelID}]}} ->
		    ListResult = io_lib:format("#{~p => {~p, [{youtubekey, ~p}, {channel_id, ~p} ] }}.~n", [Key, Name, YoutubeKey, ChannelID]),
		    ConfigRecord = list_to_binary(ListResult),
			file:write_file(File, ConfigRecord,[append])
	end.
compose_success_message(Message, Map) ->
	[Key] = maps:keys(Map),
	Value = maps:get(Key, Map),
	{Name,[{youtubekey,YoutubeKey},{channel_id,ChannelID}]} = Value,
	Result = #{client_id => Key,
		name => Name,
		youtubekey => YoutubeKey,
		channel_id => ChannelID
	},
	#{Message => Result}.

