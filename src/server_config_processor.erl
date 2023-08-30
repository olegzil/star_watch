% @Author: Oleg Zilberman
% @Date:   2023-03-06 15:30:12
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-31 18:03:23
-module(server_config_processor).
-include("include/server_config_item.hrl").
-include("include/client_profile_table.hrl").
-include("include/youtube_channel.hrl").
-include("include/macro_definitions.hrl").
-export([fetch_client_config_data_db/2, 
		 fetch_list_of_channel_ids_and_youtube_keys_db/0,
		 fetch_profile_map_from_file/1,
		 fetch_client_directory/1,
		 delete_client_config_data_db/1,
		 process_channel_request/3,
		 add_client_config_data/4,
		 fetch_profile_map_from_db/1,
		 generate_profile_map/1,
		 get_default_youtube_key/1, 
		 get_client_key/1,
		 populate_client_profile_table/1,
		 is_client_in_profile_map/1,
		 delete_config_record/1, 
		 is_channel_in_profile/2,
		 delete_youtube_channel/2,
		 fetch_config_data_for_client/1,
		 update_existing_client/4,
		 add_new_client_record/1,
		 copy_profile_and_add_new_channel/2,
		 restore_default_client/1,
		 read_private_key_file/0,
		 get_email_keys/1,
		 get_ip_flag/1,
		 update_existing_client_unconditionally/3]).

-compile(export_all).

read_private_key_file() ->
	file:read_file("private-key.pem").

parse_server_config_file(File) ->
	{ok, [ConfigMap]} = file:consult(File),
	ConfigMap.


get_qualified_file_name(File) ->
	{ok, CurrentDirectory} = file:get_cwd(),
	QualifiedFile = string:concat("/", File),
	string:concat(CurrentDirectory, QualifiedFile).


% Returns a list of maps. The map key is the client ID and the value is a list with format:  {two_pbs_space_time,[{youtubekey,<<"AIzaSyDXepMVUKYMdn9ui3Nn9X6rau37r-89t6Q">>}, 
get_profiles_list(File) ->
	MasterMap = parse_server_config_file(File),
	maps:get(client_profiles, MasterMap).

get_server_control_block(File) ->
	MasterMap = parse_server_config_file(File),
	maps:get(control_block, MasterMap).

get_default_youtube_key(File) ->
	ControlBlock = get_server_control_block(File),
	maps:get(default_youtube_api_key, ControlBlock).

get_client_key(File) ->
	ControlBlock = get_server_control_block(File),
	maps:get(default_client_key, ControlBlock).

get_email_keys(File) ->
	ControlBlock = get_server_control_block(File),
	PublicKey = maps:get(api_key_public, ControlBlock),
	SecreteKey = maps:get(api_key_private, ControlBlock),
	{PublicKey, SecreteKey}.
get_ip_flag(File) ->
	ControlBlock = get_server_control_block(File),
	maps:get(use_local_ip, ControlBlock).	
%%% Returns a map: #{directoryrecords => [Item1, ..., ItemN] }, where Item is itself a map: #{channel_id => a, client => b, name => c}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Begin fetch_channel_directory logic %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fetch_client_directory(ClientID) ->
	case mnesia:activity(transaction, fun() -> mnesia:read(client_profile_table, ClientID) end) of
		[] ->
			Message = <<"client id " , ClientID/binary,  " not found">>,
			utils:format_error(?SERVER_ERROR_INVALID_CLIENT, Message);
		[ClientRecord] ->
			ChannelList = ClientRecord#client_profile_table.channel_list,
			process_channel_request(ClientID, ChannelList, [])
	end.

process_channel_request(_Arg1, [], ListOfMaps) -> 
	Final = maps:put(directoryrecords, ListOfMaps, #{}),
	{ok, jiffy:encode(Final)};

process_channel_request(ClientID, [Channel | ChannelList], Acc) ->
	{ChannelName,ChannelID} = Channel,
	case db_access:get_channel_data(ClientID, ChannelID) of
		false ->
			process_channel_request(ClientID, ChannelList, Acc);
		Record ->
	VideoList = 
		[{video_id, Record#youtube_channel.video_id}, 
	    {url_medium, Record#youtube_channel.url_medium},
	    {title, Record#youtube_channel.title},
	    {width, Record#youtube_channel.width},
	    {height, Record#youtube_channel.height},
	    {date, utils:gregorian_days_to_binary(Record#youtube_channel.date)},
	    {channel_id, Record#youtube_channel.channel_id}],
	    LatestVideo = maps:from_list(VideoList),

	    DirectoryRecord =
	    [{channel_id, ChannelID}, 
	    	{client, ClientID}, 
	    	{name, ChannelName},
	    	{latest_video, LatestVideo}],

	    Final = maps:from_list(DirectoryRecord),
		List = lists:append(Acc, [Final]),
		process_channel_request(ClientID, ChannelList, List)		
	end.

%%% Returns a tuple whose second item is a json-friendly list of 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Begin fetch_client_config_data_db logic %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fetch_client_config_data_db(json, ClientID) ->
	ReaderFun = fun() -> mnesia:read(client_profile_table, ClientID) end,
	Result = mnesia:activity(transaction, ReaderFun),
	case Result of
		[] ->
			Message = utils:jsonify_list_of_tuples([error, client_key], [{<<"client id not found">>, ClientID}]),
			{error, Message};
		[Record] ->
			Message = utils:jsonify_client_profile_table(Record),
			{ok, Message}
	end;

fetch_client_config_data_db(not_json, ClientID) ->
	ReaderFun = fun() -> mnesia:read(client_profile_table, ClientID) end,
	Result = mnesia:activity(transaction, ReaderFun),
	case Result of
		[] ->
			{error, no_such_client};
		[Record] ->
			{ok, Record}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Begin delete_client_config_data_db logic %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_client_config_data_db(ClientID) ->
	Result = mnesia:activity(transaction, fun() -> mnesia:delete({client_profile_table, ClientID}) end),
	{ok, Result}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Begin add_client_config_data logic %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_client_config_data(ClientID, Name, YoutubeKey, ChannelID) ->
	{atomic, Result} = mnesia:transaction(fun()-> mnesia:read(client_profile_table, ClientID) end),
	case Result of
		[] ->
			Record = #client_profile_table{
				client_id = ClientID, 
				youtube_key = YoutubeKey,
				channel_list = [ChannelID]
			},
			mnesia:transaction(fun()-> mnesia:write(Record) end);
		[R] ->
			NewChannelList = R#client_profile_table.channel_list ++ [{Name, ChannelID}],
			NewRecord = R#client_profile_table{channel_list= NewChannelList},
			mnesia:transaction(fun() -> mnesia:write(NewRecord) end)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Begin fetch_profile_map_from_file logic %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns {ok,#{items =>[#{client_id => <<"ClientID">>, youtube_key=> <<"YoutubeKey">>, client_channel_data =>[{name => pbs_space_time, channel_id => <<"ChannelID">>}, ... ,#{same as before}]}
fetch_profile_map_from_file(FileName) ->
	ProfileMap = get_profiles_list(FileName),
	Keys = maps:keys(ProfileMap),
	extract_record(Keys, ProfileMap, []).

extract_record([], _Map, Acc) -> Acc;
extract_record([Key|Keys], ProfileMap, Acc) ->
	{ok, ClientProfile} = maps:find(Key, ProfileMap),
	{ok, ChannelData} = maps:find(client_channel_data, ClientProfile),
	{ok, YoutubeKey} = maps:find(youtube_api_key, ClientProfile),
	Map = #{client_id => Key, youtube_key => YoutubeKey, client_channel_data => ChannelData},
	List = lists:append(Acc, [Map]),
	extract_record(Keys, ProfileMap, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Begin fetch_profile_map_from_file logic %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Reads the database. Returns a map of clients . Each client points to a record that contains the client id and a list of channel descriptors for this client.
fetch_profile_map_from_db(Table) ->
	FetchKeys = fun() ->
		mnesia:all_keys(Table)
	end,
	KeyList = mnesia:activity(transaction, FetchKeys),
	generate_profile_map(KeyList).

generate_profile_map(Keys) ->
	AccumulatorFun = fun(Key, AccumulatorMap) ->
			ProfileItem = mnesia:activity(transaction, fun() -> mnesia:read(client_profile_table, Key) end),
			maps:put(Key, ProfileItem, AccumulatorMap)
		end,
	lists:foldr(AccumulatorFun, #{}, Keys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% Begin fetch_list_of_channel_ids_and_youtube_keys_db logic %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fetch_list_of_channel_ids_and_youtube_keys_db() ->
	KeyList = mnesia:activity(transaction, fun() -> mnesia:all_keys(client_profile_table) end),
	List = key_pair_extractor(KeyList, []),
	ListOfTuples = lists:uniq(fun(Item) -> element(2, Item) end, List),
	#{ok => ListOfTuples}.

key_pair_extractor([], Acc) -> Acc;
key_pair_extractor([Key|Remainder], Acc) ->
	ReaderFun = fun() -> mnesia:read(client_profile_table, Key) end,
	{_, [Record]} = mnesia:transaction(ReaderFun),
	{_ClientKey, YoutubeKey, ChannelData} = {Record#client_profile_table.client_id, Record#client_profile_table.youtube_key, Record#client_profile_table.channel_list},

	FoldFun = fun(Item, ListAcc) -> lists:append(ListAcc, [{YoutubeKey, element(2, Item)}]) end,
	List = lists:foldl(FoldFun, [], ChannelData),
	NewList = lists:append(Acc, List),
	key_pair_extractor(Remainder, NewList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% Begin populate_client_profile_table logic %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
populate_client_profile_table(false) -> 
	ok;
populate_client_profile_table(true) -> 
    ClientProfilesList = fetch_profile_map_from_file(?SERVER_CONFIG_FILE),
    ListFun = fun(Map) -> 
    	Record = #client_profile_table{
    		client_id = maps:get(client_id, Map), 
			youtube_key = maps:get(youtube_key, Map),
			channel_list = maps:get(client_channel_data, Map)
    	},
    	WriteFun = fun() ->
    		mnesia:write(Record)
    	end,
    	io:format("written: ~p~n", [mnesia:transaction(WriteFun)])
    end,
    lists:foreach(ListFun, ClientProfilesList).

is_client_in_profile_map(ClientID) ->
	Map = fetch_profile_map_from_db(client_profile_table),
	Keys = maps:keys(Map),
	lists:member(ClientID, Keys).

is_channel_in_profile(ClientID, ChannelID) ->
	Map = fetch_profile_map_from_db(client_profile_table),
	Keys = maps:keys(Map),
	Result = case lists:member(ClientID, Keys) of 
		 true ->
			[Profile] = maps:get(ClientID, Map),
			utils:is_key_present(ChannelID, 2, Profile#client_profile_table.channel_list);
		false ->
			false
	end,
	Result.


delete_config_record(ClientID) ->
	DelFun = fun() -> mnesia:delete({client_profile_table, ClientID}) end,

	TransactionResult = mnesia:activity(sync_transaction, 
		fun()-> 
			mnesia:delete({client_profile_table, ClientID})
		end).

delete_youtube_channel(ClientID, ChannelID) ->
	Map = fetch_profile_map_from_db(client_profile_table),
	[Profile] = maps:get(ClientID, Map),
	case lists:keyfind(ChannelID, 2, Profile#client_profile_table.channel_list) of
		{ChannelName, ChannelID} ->
			NewList = lists:delete({ChannelName, ChannelID}, Profile#client_profile_table.channel_list),
			NewProfile=Profile#client_profile_table{
				channel_list = NewList
			},
			mnesia:transaction(fun()-> mnesia:write(NewProfile) end),
			{ok, {ChannelName, ClientID}};
		false ->
			{error, no_such_channel}
	end.

fetch_config_data_for_client(ClientID)->
	{atomic, Keys} = mnesia:transaction(fun()-> mnesia:all_keys(client_profile_table) end),
	case lists:member(ClientID, Keys) of
		false ->
			{error, no_such_client};
		true ->
			{atomic, [Record]} = mnesia:transaction(fun()-> mnesia:read(client_profile_table, ClientID) end),
			{ok, Record}
	end.

add_new_client_record(NewClient) ->
	ClientID = maps:get(client_id, NewClient),
	Map = fetch_profile_map_from_db(client_profile_table),
	case maps:find(ClientID, Map) of
		error ->
			Record = #client_profile_table{
				client_id = ClientID,
				youtube_key = maps:get(youtube_api_key, NewClient),
				channel_list = maps:get(client_channel_data, NewClient)
			},
			mnesia:transaction(fun()-> mnesia:write(Record) end);
		{ok, ClientRecord} ->
			UpdatedRecrod = ClientRecord#client_profile_table.channel_list ++ maps:get(client_channel_data, NewClient),
			mnesia:transaction(fun()-> mnesia:write(UpdatedRecrod) end)
	end.

update_existing_client_unconditionally(ClientID, ChannelID, ChannelName) ->
	Map = fetch_profile_map_from_db(client_profile_table), %% fetch all client records from db
	[Record] = maps:get(ClientID, Map),	%% locate the client in question
	ChannelIdExists = lists:keyfind(ChannelID, 2, Record#client_profile_table.channel_list), %%does this client have this channel id?

	case ChannelIdExists of
		false ->	%% this channel id is new, add it
			UpdatedRecord = Record#client_profile_table{
				channel_list = Record#client_profile_table.channel_list ++ [{ChannelName, ChannelID}]
			},
			TransactionResult = mnesia:activity(sync_transaction, 
				fun()-> 
					mnesia:write(UpdatedRecord),
					youtube_data_aquisition:fetch_single_video(ClientID, ChannelID)					
				end),
			case TransactionResult of
				{ok, _Success} ->
					{ok, ChannelName, ChannelID};
				{error, Error} ->
					{error, Error}
			end;
		_ -> %% duplicate channel id
			{error, duplicate_channel}
	end.

update_existing_client(ClientID, ChannelName, ChannelID, VideoID) ->
	Map = fetch_profile_map_from_db(client_profile_table), %% fetch all client records from db
	[Record] = maps:get(ClientID, Map),	%% locate the client in question
	ChannelIdExists = lists:keyfind(ChannelID, 2, Record#client_profile_table.channel_list), %%does this client have this channel id?

	case ChannelIdExists of
		false ->	%% this channel id is new, add it
			UpdatedRecord = Record#client_profile_table{
				channel_list = Record#client_profile_table.channel_list ++ [{ChannelName, ChannelID}]
			},
			mnesia:transaction(fun()-> mnesia:write(UpdatedRecord) end),
			Key = db_access:construct_key(ChannelID, VideoID),
			case db_access:get_video_record(Key) of  %%does this video exist in the db? If it doesn't, update this channel, otherwise do nothing.
				error ->
					youtube_data_aquisition:fetch_single_video(ClientID, ChannelID);
				{ok, _Record} ->
					ok
			end,
			{ok, client_updated};

		true -> %% duplicate channel id
			{error, duplicate_channel}
	end.


%%% All arguments must be validated by the caller.
copy_profile_and_add_new_channel(TargetID, {ChannelName, ChannelID}) ->
	DefaultClient = get_client_key(?SERVER_CONFIG_FILE), % get default client key we need for coppying
	DefaultYoutubeKey = get_default_youtube_key("server_config.cfg"),
	{atomic, [DefaultRecord]} = mnesia:transaction(fun()-> mnesia:read(client_profile_table, DefaultClient) end), 
	case lists:keyfind(ChannelID, 2, DefaultRecord#client_profile_table.channel_list) of
			false ->
				NewRecord = #client_profile_table{
					client_id = TargetID, 
					youtube_key = DefaultYoutubeKey,
					channel_list = DefaultRecord#client_profile_table.channel_list ++ [{ChannelName, ChannelID}]
				},
				mnesia:transaction(fun()-> mnesia:write(NewRecord) end),
				ProfileMap = utils:config_records_to_list_of_maps([TargetID], #{TargetID => [NewRecord]}),
				{ok, ProfileMap};
	_ ->
		{error, video_link_ChannelIdExists}
	end.

restore_default_client(ClientID) ->
	case is_client_in_profile_map(ClientID) of 
		true ->
			DefaultClientID = get_client_key(?SERVER_CONFIG_FILE),
			Profiles = get_profiles_list(?SERVER_CONFIG_FILE),
			Profile = maps:get(DefaultClientID, Profiles),
			ChannelList = maps:get(client_channel_data, Profile),

			%% Just in case, restore the default client profile
			Record = #client_profile_table {
				client_id = get_profiles_list(?SERVER_CONFIG_FILE),
				youtube_key = get_default_youtube_key(?SERVER_CONFIG_FILE),
				channel_list = ChannelList
			},
			mnesia:transaction(fun()-> mnesia:write(Record) end),
				mnesia:transaction(fun() -> 
				[R] = mnesia:read(client_profile_table, ClientID),
				MergedList = lists:merge(ChannelList, R#client_profile_table.channel_list),
				UniqueList = lists:uniq(fun({_Name, ChannelID}) -> ChannelID end, MergedList),
				mnesia:write(R#client_profile_table{channel_list = UniqueList})
			end),
			fetch_client_directory(ClientID);
		_ ->
			utils:format_error(?SERVER_ERROR_NO_SUCH_CLIENT, <<"no such client id: ", ClientID/binary>>)
	end.