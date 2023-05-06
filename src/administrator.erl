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

process_item(<<"fetchclientprofile">>, Actions) ->
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
					gen_server:call(config_server, {fetchclientprofile, ClientID}, infinity)
			end
	end;

process_item(InvalidCommand, _Arg) ->
	{error, Message} = utils:format_error(?SERVER_ERROR_INVALID_COMMAND, InvalidCommand),
	{error, #{error => Message}}.

	
handle_admin_action(Action)	->
	case Action of
		{ <<"deleteconfigrecord">>, ClientID} ->
			server_config_processor:delete_config_record(ClientID),
			utils:format_success(?SERVER_ERROR_OK, <<"deleted client ", ClientID/binary, " from profile db">>);

		<<"fetchprofilemap">> ->
			MapOfRecords = server_config_processor:fetch_profile_map_from_db(),
			Keys = maps:keys(MapOfRecords),
			ProfileMap = utils:config_records_to_list_of_maps(Keys, MapOfRecords),
			{ok, ProfileMap};

		{<<"fetchclientprofile">>, ClientID} ->
			case server_config_processor:fetch_config_data_for_client(ClientID) of
				{error, ErrorMessage} -> 
					utils:format_error(ErrorMessage, <<"client profile error">>);
				{ok, ProfileRecord} ->
					ProfileMap = utils:config_records_to_list_of_maps([ClientID], #{ClientID => [ProfileRecord]}),
					{ok, ProfileMap}	
			end;
		{<<"updateclientprofile">>,  {TargetID, {ChannelName, ChannelID, VideoLink}}} ->
            case db_access:does_record_exist(TargetID, client_profile_table) of
                true ->
                    server_config_processor:add_new_channel_to_profile(TargetID, {ChannelName, ChannelID, VideoLink});
                false ->
                    server_config_processor:copy_profile_and_add_new_channel(TargetID, {ChannelName, ChannelID, VideoLink})
            end;

		Command ->
			utils:log_message([{"Command", Command}]),
			utils:format_error(?SERVER_ERROR_OK, command_not_found)
	end.