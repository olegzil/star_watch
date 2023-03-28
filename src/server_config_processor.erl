% @Author: Oleg Zilberman
% @Date:   2023-03-06 15:30:12
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-27 21:47:53
-module(server_config_processor).
-include("include/server_config_item.hrl").
-include("include/client_profile_table.hrl").
% -export([fetch_client_config_data/2, 
% 		 fetch_list_of_channel_ids_and_youtube_keys/1,
% 		 fetch_profile_map_from_file/1,
% 		 fetch_list_of_client_ids_and_channel_ids/0,
% 		 fetch_client_ids_and_names/0,
% 		 get_default_youtube_key/1, 
% 		 fetch_list_of_channel_ids_and_youtube_keys_jsonified/1,
% 		 update_client_profiles/2,
%		 populate_client_profile_table/1]).
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Begin fetch_client_config_data logic %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fetch_client_config_data(json, ClientID) ->
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
%%%%%%%%%% End fetch_client_config_data logic %%%%%%%%%%%%%%%
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
	 		io:format("client_profile_table_pending: does not contain: ~p~n", [ClientID]),
	 		Message = <<"client id: ">>,
	 		utils:format_error(<<"not found">>, <<Message/binary,ClientID/binary, " does not exist in the pending config table">>);
	 	[R] ->
	 		io:format("client_profile_table_pending: Found: ~p~n", [ClientID]),
			ClientProfile = mnesia:activity(transaction, fun() -> mnesia:read(client_profile_table, ClientID) end),
			NewClientID = R#client_profile_table_pending.client_id,
			NewChannelList = R#client_profile_table_pending.channel_list,
			NewRecord = #client_profile_table_pending{client_id = NewClientID, channel_list = NewChannelList},
			update_client_profile_channels(ClientID, ClientProfile, NewRecord)
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
%% Returns {ok,#{items =>[#{channel_id => <<"ChannelID">>,client_id => <<"ClientID">>,name => pbs_space_time,youtube_id => <<"YoutubeKey">>}, ... ,#{same as before}]
fetch_profile_map_from_file(FileName) ->
	List = get_profiles_list(FileName),
	{ok, #{items => extract_record(List, #{})}}.

extract_record([], Acc) -> Acc;
extract_record([Map|Maps], Acc) ->
	NewMap = maps:merge(Acc, Map),
	extract_record(Maps, NewMap).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% End fetch_profile_map_from_file logic %%%%%%%%%%
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
%%%%%%%%%%%% End fetch_profile_map_from_file logic %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fetch_list_of_channel_ids_and_youtube_keys_jsonified(FileName) ->
	ListOfMaps = get_profiles_list(FileName),
	{TupleList} = fetch_client_and_youtube_ids(ListOfMaps, []),
	{ok, utils:jsonify_list_of_tuples([youtube_key, channel_id], TupleList)}.

fetch_list_of_channel_ids_and_youtube_keys(FileName) ->
	List = get_profiles_list(FileName),
	#{ok => fetch_client_and_youtube_ids(List, [])}.


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

populate_client_profile_table(false) -> ok;
populate_client_profile_table(true) -> 
    mnesia:wait_for_tables([client_profile_table], 30000),	
    {ok, ClientProfilesMap} = fetch_profile_map_from_file("server_config.cfg"),
    [ProfileMap] = maps:values(ClientProfilesMap),
    Keys = maps:keys(ProfileMap),
    update_db_with_client_profile(Keys, ProfileMap).

update_db_with_client_profile(Keys, ProfileMap) ->
    Fun = fun(Key) ->  
        UpdateFun = fun() ->
            ConfigRecord = #client_profile_table{
                client_id   = Key,
                channel_list = maps:get(Key, ProfileMap)
            },
            mnesia:write(ConfigRecord)
        end,
        mnesia:activity(transaction, UpdateFun)
    end,
    lists:foreach(Fun, Keys).

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

