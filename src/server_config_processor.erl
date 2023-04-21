% @Author: Oleg Zilberman
% @Date:   2023-03-06 15:30:12
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-31 18:03:23
-module(server_config_processor).
-include("include/server_config_item.hrl").
-include("include/client_profile_table.hrl").
-include("include/youtube_channel.hrl").
-export([fetch_client_config_data_db/2, 
		 fetch_list_of_channel_ids_and_youtube_keys_db/0,
		 fetch_profile_map_from_file/1,
		 fetch_channel_directory/1,
		 process_channel_request/3,
		 add_client_pending_config_data/4,
		 promote_client_pending_config_data/1,
		 update_client_profile_channels/3,
		 update_client_record/3,
		 fetch_profile_map_from_db/0,
		 generate_profile_map/1,
		 get_default_youtube_key/1, 
		 get_client_key/1,
		 populate_client_profile_table/1]).

parse_server_config_file(File) ->
	{ok, [ConfigMap]} = file:consult(File),
	ConfigMap.

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


%%% Returns a map: #{directoryrecords => [Item1, ..., ItemN] }, where Item is itself a map: #{channel_id => a, client => b, name => c}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Begin fetch_channel_directory logic %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fetch_channel_directory(ClientID) ->
	case mnesia:activity(transaction, fun() -> mnesia:read(client_profile_table, ClientID) end) of
		[] ->
			Message = utils:jsonify_list_of_tuples([error, client_key], [{<<"client id not found">>, ClientID}]),
			{error, Message};
		[ClientRecord] ->
			ChannelList = ClientRecord#client_profile_table.channel_list,
			process_channel_request(ClientID, ChannelList, [])
	end.

process_channel_request(_Arg1, [], ListOfMaps) -> 
	Final = maps:put(directoryrecords, ListOfMaps, #{}),
	{ok, jiffy:encode(Final)};

process_channel_request(ClientID, [Channel | ChannelList], Acc) ->
	{ChannelName,[{_,_},{_,ChannelID}]} = Channel,
	A = maps:merge(maps:put(channel_id, ChannelID, #{}), maps:put(client, ClientID, #{})),
	Final = maps:merge(A, maps:put(name, ChannelName, A)),
	List = lists:append(Acc, [Final]),
	process_channel_request(ClientID, ChannelList, List).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% End fetch_channel_directory logic %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
		ConfigRecord ->
			[Record] = ConfigRecord,
			#client_profile_table{
				client_id = ClientID
			} = Record,

			Message = utils:jsonify_client_profile_table(#{ClientID => ConfigRecord}),
			{ok, Message}
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% End fetch_client_config_data_db logic %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Begin add_client_pending_config_data logic %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_client_pending_config_data(ClientID, Name, YoutubeKey, ChannelID) ->
	NewRecord = #client_profile_table_pending{
		client_id = ClientID,
		channel_list = [{Name,[{youtubekey,YoutubeKey},{channel_id,ChannelID}]}]
	},
	WriteFun = fun() -> mnesia:write(NewRecord) end,
	mnesia:activity(transaction, WriteFun),
	{ok, record_added}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% End add_client_pending_config_data logic %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

promote_client_pending_config_data(ClientID) ->
	ReaderFun = fun() -> mnesia:read(client_profile_table_pending, ClientID)	end,
	case mnesia:activity(transaction, ReaderFun) of
	 	[] ->
	 		Message = <<"client id: ">>,
	 		utils:format_error(<<"not found">>, <<Message/binary,ClientID/binary, " does not exist in the pending config table">>);
	 	[R] ->
			ClientProfile = mnesia:activity(transaction, fun() -> mnesia:read(client_profile_table, ClientID) end),
			NewClientID = R#client_profile_table_pending.client_id,
			NewChannelList = R#client_profile_table_pending.channel_list,
			NewRecord = #client_profile_table_pending{client_id = NewClientID, channel_list = NewChannelList},
			update_client_profile_channels(ClientID, ClientProfile, NewRecord),
			mnesia:activity(transaction, fun() -> mnesia:delete({client_profile_table_pending, ClientID}) end),
			Message = <<"client id: ">>,
	 		utils:format_error(<<"not found">>, <<Message/binary,ClientID/binary, " promoted to production and deleted from pending table">>)
	 end .

%%% The production profile table does not have this client. Add it unconditionally
update_client_profile_channels(_ClientID, [], NewRecord) ->
	OutRecord = #client_profile_table{client_id = NewRecord#client_profile_table_pending.client_id,	
																			 channel_list = NewRecord#client_profile_table_pending.channel_list},
	mnesia:activity(transaction, fun() -> mnesia:write(OutRecord) end),
	{ok, record_promoted_1};

%%% The Production profile table does have this client. Search its chanel list and update it accordingly.
update_client_profile_channels(_ChannelID, [ClientProfile], NewRecord) ->
	[{NewName,[{_,_},{_,_}]}] = NewRecord#client_profile_table_pending.channel_list,
	Found = lists:any(fun(Item)-> 
		{ChannelName,[{_,_},{_,_}]} = Item,
		ChannelName =:= NewName
	end, NewRecord#client_profile_table_pending.channel_list),
	update_client_record(Found, ClientProfile, NewRecord).

%%% The Client channel list did not contain this channel name. Append it to the channel list and write it.
update_client_record(false, _ClientProfile, NewRecord) ->
	mnesia:activity(transaction, fun() -> mnesia:write(NewRecord) end),
	{ok, record_promoted_2};


%%% The Client channel list did contian the target channel name. Update the specific entry in the channel list and write it.
%%% The update is performed by first removing the target channel and then appending the updated channel.
update_client_record(true, ClientProfile, NewRecord) ->
	{_,ClientID,[{NewName,[{_,_},{_,_}]}]}  = NewRecord,
	TargetList = ClientProfile#client_profile_table.channel_list,
	AccumulatorFun = fun(OldRecord, Acc) -> 
		WorkRecord = OldRecord,
		{OldName,[{_,_},{_,_}]} = WorkRecord,
		case OldName =/= NewName of
			true ->
				lists:append(Acc, [OldRecord]);
			false -> Acc
		end
	end,
	UpdatedList = lists:foldr(AccumulatorFun, [], TargetList),
	case is_list(UpdatedList) of
		true ->
			FinalList = lists:append(UpdatedList, NewRecord#client_profile_table_pending.channel_list),
			mnesia:activity(transaction, fun() -> mnesia:write(#client_profile_table{client_id=ClientID, channel_list = FinalList}) end);
		false ->
			FinalList = lists:append([UpdatedList], NewRecord#client_profile_table_pending.channel_list),
			mnesia:activity(transaction, fun() -> mnesia:write(#client_profile_table{client_id=ClientID, channel_list = FinalList}) end)
	end,
	{ok, record_promoted_3}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Begin fetch_profile_map_from_file logic %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns {ok,#{items =>[#{client_id => <<"ClientID">>, youtube_key=> <<"YoutubeKey">>, channel_data =>[{name => pbs_space_time, channel_id => <<"ChannelID">>}, ... ,#{same as before}]}
fetch_profile_map_from_file(FileName) ->
	ProfileMap = get_profiles_list(FileName),
	Keys = maps:keys(ProfileMap),
	extract_record(Keys, ProfileMap, []).

extract_record([], _Map, Acc) -> Acc;
extract_record([Key|Keys], ProfileMap, Acc) ->
	{ok, ClientProfile} = maps:find(Key, ProfileMap),
	{ok, ChannelData} = maps:find(client_channel_data, ClientProfile),
	{ok, YoutubeKey} = maps:find(youtube_api_key, ClientProfile),
	Map = #{client_id => Key, youtube_key => YoutubeKey, channel_data => ChannelData},
	List = lists:append(Acc, [Map]),
	extract_record(Keys, ProfileMap, List).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% End fetch_profile_map_from_db logic %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Begin fetch_profile_map_from_file logic %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Reads the database. Returns a map of clients . Each client points to a record that contains the client id and a list of channel descriptors for this client.
fetch_profile_map_from_db() ->
	FetchKeys = fun() ->
		mnesia:all_keys(client_profile_table)
	end,
	KeyList = mnesia:activity(transaction, FetchKeys),
	generate_profile_map(KeyList).

generate_profile_map(Keys) ->
	AccumulatorFun = fun(Key, AccumulatorMap) ->
			ProfileItem = mnesia:activity(transaction, fun() -> mnesia:read(client_profile_table, Key) end),
			maps:put(Key, ProfileItem, AccumulatorMap)
		end,
	lists:foldr(AccumulatorFun, #{}, Keys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% End fetch_profile_map_from_file logic %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% Begin fetch_list_of_channel_ids_and_youtube_keys_db logic %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fetch_list_of_channel_ids_and_youtube_keys_db() ->
	KeyList = mnesia:activity(transaction, fun() -> mnesia:all_keys(client_profile_table) end),
	List = key_pair_extractor(KeyList, []),
	ListOfTuples = lists:uniq(fun(Item) -> element(2, Item) end, List),
	io:format("~n*********************************************~n"),
	io:format("ListOfTuples: ~p~n", [ListOfTuples]),
	io:format("*********************************************~n"),
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
%%%%%%%%%%%% End fetch_list_of_channel_ids_and_youtube_keys_db logic %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% Begin populate_client_profile_table logic %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
populate_client_profile_table(false) -> 
	io:format("~n*********************************************~n"),
	io:format("populate_client_profile_table: FALSE~n"),
	io:format("*********************************************~n"),
	ok;
populate_client_profile_table(true) -> 
    ClientProfilesList = fetch_profile_map_from_file("server_config.cfg"),
    ListFun = fun(Map) -> 
    	Record = #client_profile_table{
    		client_id = maps:get(client_id, Map), 
			youtube_key = maps:get(youtube_key, Map),
			channel_list = maps:get(channel_data, Map)
    	},
    	WriteFun = fun() ->
    		mnesia:write(Record)
    	end,
    	io:format("writen: ~p~n", [mnesia:transaction(WriteFun)]) 
    end,
    lists:foreach(ListFun, ClientProfilesList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% End populate_client_profile_table logic %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
