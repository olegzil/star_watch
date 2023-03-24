% @Author: Oleg Zilberman
% @Date:   2023-03-06 15:30:12
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-23 18:59:02
-module(server_config_processor).
-include("include/server_config_item.hrl").
% -export([fetch_client_config_data/2, 
% 		 fetch_list_of_channel_ids_and_youtube_keys/1,
% 		 fetch_profile_map/1,
% 		 fetch_list_of_client_ids_and_channel_ids/0,
% 		 fetch_client_ids_and_names/0,
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

% Returns a list of maps. The map key is the client ID and the value is a list with format:  {two_pbs_space_time,[{youtubekey,<<"AIzaSyDXepMVUKYMdn9ui3Nn9X6rau37r-89t6Q">>}, 
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

%%% Returns a tuple whose second item is a json-friendly list of 
fetch_client_config_data(File,ClientID) ->
	Result = fetch_client_config_data_private(File, ClientID),
	case Result of
		error ->
			Message = utils:jsonify_list_of_tuples([error, client_key], [{<<"client id not found">>, ClientID}]),
			{error, Message};
		ListOfRecords ->
			Message = generate_jsonified_list(ClientID, ListOfRecords, []),
			{ok, Message}
	end.

%%% Returns a list of records
fetch_client_config_data_private(File, ClientID) ->
	List = get_profiles_list(File), 
	Result = fetch_data_for_key(ClientID, List),
	case Result of
		error ->
			error;
		ListOfTuples ->
			generate_list_of_records(ClientID, ListOfTuples, [])
	end.

generate_jsonified_list(_Arg1, [], Acc) -> Acc;
generate_jsonified_list(ClientID, [Record|Records], Acc) ->
	Item = utils:jsonify_list_of_tuples([client_key, name, youtubekey, channel_id], 
		[{ClientID, Record#server_config_item.name, Record#server_config_item.youtubekey, Record#server_config_item.channel_id}]),
	generate_jsonified_list(ClientID, Records, lists:append(Acc, Item)).

generate_list_of_records(_Arg1, [], Acc) ->
	Acc;
generate_list_of_records(ClientID, [Item|Records], Acc) ->
	{Name, [{_, YoutubeKey}, {_, ChannelID} ]} = Item,
	Record = #server_config_item{
		name = Name,
		client_id = ClientID, 
		youtubekey = YoutubeKey,
		channel_id = ChannelID
	},
	generate_list_of_records(ClientID, Records, lists:append(Acc, [Record])).


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
	ListOfMaps = get_profiles_list(FileName),
	{TupleList} = fetch_client_and_youtube_ids(ListOfMaps, []),
	{ok, utils:jsonify_list_of_tuples([youtube_key, channel_id], TupleList)}.

fetch_client_and_youtube_ids([], Acc) -> {Acc};

fetch_client_and_youtube_ids([Client|Tail], Acc) ->
	[Value] = maps:values(Client),
	UpdatedList = extract_profile_entry_from_list(Value, []),
	fetch_client_and_youtube_ids(Tail, lists:append(Acc, UpdatedList)).
	
extract_profile_entry_from_list([], Acc) -> Acc;
extract_profile_entry_from_list([Item|List], Acc) ->
	{_, [{_, YoutubeKey}, {_, ChannelId} ]} = Item,
	UpdatedList = lists:append(Acc, [{YoutubeKey, ChannelId}]),
	extract_profile_entry_from_list(List, UpdatedList).

%%% Returns a list of items where each item is {two_pbs_space_time,[{youtubekey,<<"AIzaSyDXepMVUKYMdn9ui3Nn9X6rau37r-89t6Q">>},{channel_id,<<"UC7_gcs09iThXybpVgjHZ_7g">>}]}
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

