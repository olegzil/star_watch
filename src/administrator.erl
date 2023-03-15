% @Author: Oleg Zilberman
% @Date:   2023-03-08 19:03:08
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-14 20:15:17
-module(administrator).
-include ("include/admin_response.hrl").
-include("include/macro_definitions.hrl").
-export([execute_action/1]).

execute_action(Request) ->
	[A, Subject] = string:split(Request, ":"),
	Verb = binary_to_atom(A),
	case {Verb, Subject} of
		{channel_directory, <<"fetch">>} ->
			{ok, jiffy:encode( server_config_processor:fetch_client_ids_and_names())};
		{channel_directory, Subject} ->
			Result = validate_action(Subject),
			{ok, jiffy:encode(Result)};
		_ ->
			Message = <<"No such action: ">>,
			{error, jiffy:encode( #{<<"error">> => <<Message/binary, Subject/binary>>})}
	end.

validate_action(Verb) ->
	% Request = <<"channel_directory:add:id=cad00c93-b012-49f9-97f9-35e69ae083a0:name=sonomaashram:youtubekey=AIzaSyAHvRD_wu1xR8D_fmJwkiPO0jqw_rnhvHQ:channel_id=UCQfZkf3-Y2RwzdRFWXYsdaQ">>.
	%% if yutubekey is omited, the default is used.
	Parts = string:split(Verb, ":", all),
	%% expected result:  [<<"the action requested">>, "id=the client id", "name=the name", "youtubekey:the key, maybe", "channel_id=the channel id"]
	process_verb(Parts).

process_verb([Head|Tail]) ->
	process_item(string:lowercase(Head), Tail).

process_item(<<"delete">>, Actions) -> 
	[Value] = Actions,
	Parts = string:split(Value, "="),
	ClientID = lists:nth(2, Parts),
	server_config_processor:delete_record(?SERVER_CONFIG_FILE, ClientID);

process_item(<<"fetchchannel_ids_youtube_keys">>, _Actions) ->
	Pid = global:whereis_name(server_config),
	gen_server:call(Pid, {fetchlistofchannelidsandyoutubekeys}, infinity);

process_item(<<"fetchprofilemap">>, _Actions) ->
	Pid = global:whereis_name(server_config),
	gen_server:call(Pid, {fetchprofilemap}, infinity);

process_item(<<"fetch">>, _Actions) ->
	server_config_processor:fetch_client_ids_and_names();

process_item(<<"add">>, Actions) ->
	Result = validate_command_string(Actions, #{}),
	case Result of
		{ok, Map} ->
			IdKey = maps:get(<<"id">>, Map),
			NameKey = binary_to_atom(maps:get(<<"name">>, Map)), 
			ChannelID = maps:get(<<"channel_id">>, Map),

			Test = maps:is_key(?YOUTUBE_KEY, Map),
			if
				Test =:= true ->
					NewRecord = #{IdKey => {NameKey, [{youtubekey, maps:get(?YOUTUBE_KEY, Map)}, {channel_id, ChannelID} ] }},
					gen_server:call(global:whereis_name(server_config), {addconfigrecord, NewRecord}, infinity);

				true ->
					NewRecord = #{IdKey => {NameKey, [{youtubekey, ?DEFAULT_YOUTUBE_KEY}, {channel_id, ChannelID} ] }},
					gen_server:call(global:whereis_name(server_config), {addconfigrecord, NewRecord}, infinity)

			end;

		{error, Message} ->
			{error, Message}
	end.
	
validate_command_string([], Acc) -> 
	Test = maps:is_key(<<"id">>, Acc) andalso
		maps:is_key(<<"name">>, Acc) andalso
		maps:is_key(<<"channel_id">>, Acc),
	if
		 Test =:= true ->
			{ok, Acc};
		true -> 
			Keys=maps:keys(Acc),
			format_error(<<"Missing required key: ">>, list_to_binary(Keys))
	end;

validate_command_string([Head|Tail], Acc) ->
	Items = string:split(Head, <<"=">>),
	Key = lists:nth(1, Items),
	if
		length(Items) < 2 ->
			format_error(<<"Missing key: ">>, list_to_binary(Items));
			true -> 
				case maps:is_key(Key, Acc) of
					true -> format_error(<<"Duplicate key">>, Key);
					false -> 
						NewMap = maps:put(Key, lists:nth(2, Items), Acc),
						validate_command_string(Tail, NewMap)		
				end
	end.

format_error(ErrorType, ErrorMessage) ->
	{error, <<ErrorType/binary, ErrorMessage/binary>>}.

