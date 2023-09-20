% @Author: Oleg Zilberman
% @Date:   2023-03-08 19:03:08
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-28 17:27:37
-module(administrator).
-include ("include/admin_response.hrl").
-include("include/macro_definitions.hrl").
-include("include/invalid_request.hrl").
-include("include/client_profile_table.hrl").
-export([handle_admin_action/2]).

handle_admin_action(Action, Parameter)	->
	case Action of
		<<"refreshclietprofiles">> ->
			server_config_processor:populate_client_profile_table(client_channel_data, true),
			server_config_processor:populate_client_profile_table(client_video_data, true),
			gen_server:cast(db_access_server, refreshclietprofiles),
			utils:format_success(?SERVER_ERROR_OK, <<"client profiles updated with contents of server_cofnig.cfg">>);

		<<"addchannel">> ->
			{ClientID, ChannelID, ChannelName} = Parameter,
			case server_config_processor:update_existing_client_unconditionally(ClientID, ChannelID, ChannelName) of
				{ok, ChannelName, ChannelID} ->
					utils:format_success(?SERVER_ERROR_OK, <<"added channel name: ", ChannelName/binary, " with channel id: ", ChannelID/binary, " to client: ", ClientID/binary >>);
				{error, ResponseCode} ->
					{_, Code} = lists:keyfind(ResponseCode, 1, ?RESPONSE_CODES),
					utils:format_error(Code, <<"error adding channel id: ", ChannelID/binary, " for client: ", ClientID/binary>>)
			end;

		<<"deleteyoutubechannel">> ->
			{ClientID, ChannelID} = Parameter,
			case server_config_processor:delete_youtube_channel(ClientID, ChannelID) of
				{ok, {ChannelName, ClientID}} ->
					utils:format_success(?SERVER_ERROR_OK, <<"deleted chanel ", ChannelName/binary, " for client ", ClientID/binary>>);
				{error, ResponseCode} ->
					{_, Code} = lists:keyfind(ResponseCode, 1, ?RESPONSE_CODES),
					utils:format_error(Code, <<"error deleting chanel ", ChannelID/binary, " for client ", ClientID/binary>>)
			end;

		<<"deleteconfigrecord">> ->
			{ClientID, _ChannelID} = Parameter,
			server_config_processor:delete_config_record(ClientID),
			utils:format_success(?SERVER_ERROR_OK, <<"deleted client ", ClientID/binary, " from profile db">>);

		<<"fetchprofilemap">> ->
			MapOfRecords = server_config_processor:fetch_profile_map_from_db(client_profile_table),
			Keys = maps:keys(MapOfRecords),
			ProfileMap = utils:config_records_to_list_of_maps(Keys, MapOfRecords),
			{ok, ProfileMap};

		<<"fetchclientprofile">> ->
			{ClientID, _ChannelID} = Parameter,
			case server_config_processor:fetch_config_data_for_client(ClientID) of
				{error, ErrorMessage} -> 
					utils:format_error(ErrorMessage, <<"client profile error">>);
				{ok, ProfileRecord} ->
					ProfileMap = utils:config_records_to_list_of_maps([ClientID], #{ClientID => [ProfileRecord]}),
					{ok, ProfileMap}	
			end;
		Command ->
			utils:log_message([{"handle_admin_action failed for Action", Command}]),
			utils:format_error(?SERVER_ERROR_OK, command_not_found)
	end.