% @Author: Oleg Zilberman
% @Date:   2023-03-08 19:03:08
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-28 17:27:37
-module(administrator).
-include ("include/admin_response.hrl").
-include("include/macro_definitions.hrl").
-include("include/invalid_request.hrl").
-include("include/client_profile_table.hrl").
-export([execute_action/1, handle_admin_action/1]).

execute_action(Request) ->
	[A, Subject] = string:split(Request, ":"),
	Verb = binary_to_atom(A),
	io:format("~nRequest: ~p~nSubject: ~p~nVerb: ~p~n", [Request, Subject, Verb]),	
	case {Verb, Subject} of
		{channel_directory, Subject} ->
			{_, Result} = validate_action(Subject),
			{ok, jiffy:encode(Result)};
		_ ->
			Message = <<"No such action: ">>,
			{error, jiffy:encode( #{<<"error">> => <<Message/binary, Subject/binary>>})}
	end.

validate_action(Verb) ->
	%% if yutubekey is omited, the default is used.
	case string:find(Verb, ":") of
		nomatch ->
			process_item(Verb, []);
		_ ->
			[Action, Subject] = string:split(Verb, ":"),
			process_item(string:lowercase(Action), Subject)
	end.

process_item(<<"deleteconfigrecord">>, Actions) -> 
	Value = Actions,
	Parts = string:split(Value, "="),
	if
		length(Parts) < 2 ->
			{error, Message} = utils:format_error(?SERVER_ERROR_BAD_CLIENT_ID, <<"requires a client id ">>),
			{error, #{error => Message}};
		true ->
			ClientID = lists:nth(2, Parts),
			Result = gen_server:call(config_server, {deleteconfigrecord, ClientID}, infinity),
			Result
	end;

process_item(<<"fetchprofilemap">>, _Actions) ->
	gen_server:call(config_server, {fetchprofilemap}, infinity);

process_item(<<"fetchclientconfigdata">>, Actions) ->
	if
		length(Actions) =:= 0 ->
			{error, Message} = utils:format_error(?SERVER_ERROR_MISSING_ACTION, requires_action_token),
			{error, #{error => Message}};
		true ->
			Value = Actions,
			Parts = string:split(Value, "="),
			Action = string:lowercase(lists:nth(1, Parts)),
			ClientID = lists:nth(2, Parts),
			case Action =:= <<"client_id">> of
				false ->
					{error, Message} = utils:format_error(?SERVER_ERROR_BAD_CLIENT_ID, requires_client_id),
					{error, #{error => Message}};
				true ->
					gen_server:call(config_server, {fetchclientconfigdata, ClientID}, infinity)
			end
	end;

process_item(<<"promoteconfigrecord">>, Actions) ->
	if
		length(Actions) =:= 0 ->
			{error, Message} = utils:format_error(?SERVER_ERROR_MISSING_ACTION, requires_action_token),
			{error, #{error => Message}};
		true ->
			Value = Actions,
			Parts = string:split(Value, "="),
			Action = string:lowercase(lists:nth(1, Parts)),
			ClientID = lists:nth(2, Parts),
			case Action =:= <<"client_id">> of
				false ->
					{error, Message} = utils:format_error(?SERVER_ERROR_BAD_CLIENT_ID, requires_client_id),
					{error, #{error => Message}};
				true ->
					gen_server:call(config_server, {promoteconfigrecord, ClientID}, infinity)
			end
	end;

process_item(<<"add">>, Actions) ->
	Parts = string:split(Actions, ",", all),
	Result = validate_add_command(Actions, Parts, #{}),
	case Result of
		{ok, Map} ->
			ClientID = maps:get(<<"client_id">>, Map),
			Name = binary_to_atom(maps:get(<<"name">>, Map)), 
			ChannelID = maps:get(<<"channel_id">>, Map),

			Test = maps:is_key(?YOUTUBE_KEY, Map),
			if
				Test =:= true ->
					YoutubeKey = maps:get(?YOUTUBE_KEY, Map),
					gen_server:call(config_server, {addconfigrecord, ClientID, Name, YoutubeKey, ChannelID}, infinity);
				true ->
					YoutubeKey = server_config_processor:get_default_youtube_key("server_config.cfg"),
					gen_server:call(config_server, {addconfigrecord, ClientID, Name, YoutubeKey, ChannelID}, infinity)
			end;

		{error, Message} ->
			{error, #{error => Message}}
	end;
process_item(InvalidCommand, _Arg) ->
	{error, Message} = utils:format_error(?SERVER_ERROR_INVALID_COMMAND, InvalidCommand),
	{error, #{error => Message}}.

	
validate_commands([], _Arg2, Map) ->
	{ok, Map};
validate_commands([Command|Tail], CommandList, Map) ->
	case lists:member(Command, CommandList) of
		true ->
			validate_commands(Tail, CommandList, Map);
		false ->
			utils:format_error(?SERVER_ERROR_MALFORMED_COMMAND, Command)
	end.

validate_add_command(_Arg1, [], Map) -> 
	CommandList = [<<"client_id">>, <<"name">>, <<"channel_id">>],
	validate_commands(CommandList, maps:keys(Map), Map);

validate_add_command(Actions, [Head|Tail], Map) ->
	Items = string:split(Head, <<"=">>),
	Label = lists:nth(1, Items),
	if
		length(Items) < 2 ->
			utils:format_error(?SERVER_ERROR_MALFORMED_COMMAND, add);
			true -> 
				Value = lists:nth(2, Items),
				case Label of
					<<"client_id">> ->
						NewMap = maps:put(Label, Value, Map),
						validate_add_command(Actions, Tail, NewMap);
					<<"name">> ->
						NewMap = maps:put(Label, Value, Map),
						validate_add_command(Actions, Tail, NewMap);
					<<"youtubekey">> ->
						NewMap = maps:put(Label, Value, Map),
						validate_add_command(Actions, Tail, NewMap);
					<<"channel_id">> ->
						NewMap = maps:put(Label, Value, Map),
						validate_add_command(Actions, Tail, NewMap);
					_ ->
						validate_add_command(Actions, Tail, Map)
				end
	end.

handle_admin_action(Action)	->
	case Action of
		{ <<"deleteconfigrecord">>, ClientID} ->
			server_config_processor:delete_config_record(ClientID),
			utils:format_success(<<"deleted client ", ClientID/binary, " from profile db">>);
		{<<"deleteyoutubechannel">>, {ClientID, ChannelID}} ->
			case server_config_processor:delete_youtube_channel(ClientID, ChannelID) of 
				{atomic, ok} ->
					utils:format_success(<<"deleted: ", ChannelID/binary, " for client ", ClientID/binary>>);
				_ ->
					utils:format_error(-1, unknow)
			end;

		<<"fetchprofilemap">> ->
			MapOfRecords = server_config_processor:fetch_profile_map_from_db(),
			Keys = maps:keys(MapOfRecords),
			ProfileMap = lists:foldl(fun(Key, Acc)-> 
				[R] = maps:get(Key, MapOfRecords),
				RecordMap = #{
					client_id => Key,
					youtube_key => R#client_profile_table.youtube_key,
					channel_list => utils:tuple_list_to_list_of_maps({channel_name, channel_id}, R#client_profile_table.channel_list)
				},				
				Acc ++ [RecordMap]
			end,
			[], Keys),
			{ok, ProfileMap};
		_ ->
			utils:format_error(?SERVER_ERROR_OK, command_not_found)
	end.