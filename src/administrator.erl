% @Author: Oleg Zilberman
% @Date:   2023-03-08 19:03:08
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-13 17:30:48
-module(administrator).
-include ("include/admin_response.hrl").
-include("include/macro_definitions.hrl").
-export([execute_action/1]).

execute_action(Request) ->
	% http://10.0.0.18:8083/youtube/admin/?key=f09a2270-ac19-418c-a443-9f9e4f4c9019&action=channel_directory:add:id=cad00c93-b012-49f9-97f9-35e69ae083a0:name=sonomaashram:youtubekey=AIzaSyAHvRD_wu1xR8D_fmJwkiPO0jqw_rnhvHQ:channel_id=UCQfZkf3-Y2RwzdRFWXYsdaQ
	[A, Subject] = string:split(Request, ":"),
	Verb = binary_to_atom(A),
	case {Verb, Subject} of
		{channel_directory, <<"fetch">>} ->
			{ok, jiffy:encode( server_config_processor:fetch_client_ids_and_names())};
		{channel_directory, Subject} ->
			Message = validate_action(Subject),
			JsonFreindly = jsonify_message(Message),
			io:format("JsonFreindly: ~p~n", [JsonFreindly]),
			{ok, jiffy:encode(#{<<"result">> => JsonFreindly})};
		_ ->
			Message = <<"No such action: ">>,
			{error, jiffy:encode( #{<<"error">> => <<Message/binary, Subject/binary>>})}
	end.

validate_action(Verb) ->
	% Request = <<"channel_directory:add:id=cad00c93-b012-49f9-97f9-35e69ae083a0:name=sonomaashram:youtubekey=AIzaSyAHvRD_wu1xR8D_fmJwkiPO0jqw_rnhvHQ:channel_id=UCQfZkf3-Y2RwzdRFWXYsdaQ">>.
	% administrator:execute_action(Request).
	%% expected string:
	%% <<"channel_directory:fetch">>
	%% <<"delete:cad00c93-b012-49f9-97f9-35e69ae083a0">>
	%% <<"add:id=cad00c93-b012-49f9-97f9-35e69ae083a0:name=sonomaashram:youtubekey:AIzaSyAHvRD_wu1xR8D_fmJwkiPO0jqw_rnhvHQ:channel_id=UCQfZkf3-Y2RwzdRFWXYsdaQ">>.
	%% if yutubekey is omited, the default is used.
	Parts = string:split(Verb, ":", all),
	%% expected result:  [<<"add">>, "id=the client id", "name=the name", "youtubekey:the key, maybe", "channel_id=the channel id"]
	process_verb(Parts).

process_verb([Head|Tail]) ->
	process_item(string:lowercase(Head), Tail).

process_item(<<"delete">>, _Actions) -> 
	Error = {action_not_implemented, delete},
	{[Error]};
process_item(<<"fetch">>, _Actions) ->
	server_config_processor:fetch_client_ids_and_names();

process_item(<<"add">>, Actions) ->
	Result = validate_command_string(Actions, #{}),
	case Result of
		{ok, Map} ->
			IdKey = maps:get(<<"id">>, Map),
			NameKey = binary_to_atom(maps:get(<<"name">>, Map)), 
			ChannelID = maps:get(<<"channel_id">>, Map),

			Test = maps:is_key(<<"youtubekey">>, Map),
			if
				Test =:= true ->
					NewRecord = #{IdKey => {NameKey, [{youtubekey, maps:get(?YOUTUBEKEY, Map)}, {channel_id, ChannelID} ] }},
					gen_server:call(server_config, {addconfigrecord, NewRecord}, infinity);

				true ->
					NewRecord = #{IdKey => {NameKey, [{youtubekey, ?DEFAULT_YOUTUBE_KEY}, {channel_id, ChannelID} ] }},
					gen_server:call(server_config, {addconfigrecord, NewRecord}, infinity)

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

jsonify_message_parts(Name, [], Acc) -> 
	{[{Name, Acc}]};

jsonify_message_parts(Name, [Head|Tail], Acc) ->
	A = {[Head]},
	jsonify_message_parts(Name, Tail, lists:append(Acc, [A])).
jsonify_message({ErrorMessage}) -> {ErrorMessage};
jsonify_message(Map) ->
	[Key] = maps:keys(Map),
	Value = maps:get(Key, Map), % returns a tuple.
	{Name, List} = Value,
	#{Key => jsonify_message_parts(Name, List, [])}.

