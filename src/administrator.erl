% @Author: Oleg Zilberman
% @Date:   2023-03-08 19:03:08
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-18 12:47:35
-module(administrator).
-include ("include/admin_response.hrl").
-include("include/macro_definitions.hrl").
-export([execute_action/1]).

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
	% Request = <<"channel_directory:add:id=cad00c93-b012-49f9-97f9-35e69ae083a0:name=sonomaashram,youtubekey=AIzaSyAHvRD_wu1xR8D_fmJwkiPO0jqw_rnhvHQ,channel_id=UCQfZkf3-Y2RwzdRFWXYsdaQ">>.
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
			{error, Message} = format_error(<<"deleteconfigrecord">>, <<" requires a client id ">>),
			{error, #{error => Message}};
		true ->
			ClientID = lists:nth(2, Parts),
			Pid = global:whereis_name(server_config),
			Result = gen_server:call(Pid, {deleteconfigrecord, ClientID}, infinity),
			Result
	end;

process_item(<<"fetchlistofchannelidsandyoutubekeys">>, _Actions) ->
	Pid = global:whereis_name(server_config),
	gen_server:call(Pid, {fetchlistofchannelidsandyoutubekeys}, infinity);

process_item(<<"fetchprofilemap">>, _Actions) ->
	Pid = global:whereis_name(server_config),
	gen_server:call(Pid, {fetchprofilemap}, infinity);

process_item(<<"fetchclientconfigdata">>, Actions) ->
	if
		length(Actions) =:= 0 ->
			{error, Message} = format_error(<<"fetchclientconfigdata">>, <<" requires a client id ">>),
			{error, #{error => Message}};
		true ->
			Value = Actions,
			Parts = string:split(Value, "="),
			ClientID = lists:nth(2, Parts),
			Pid = global:whereis_name(server_config),
			gen_server:call(Pid, {fetchclientconfigdata, ClientID}, infinity)
	end;

process_item(<<"fetchlistofclientidsandchannelids">>, _Actions) ->
	Pid = global:whereis_name(server_config),
	gen_server:call(Pid, {fetchlistofclientidsandchannelids}, infinity);

process_item(<<"fetchclientidsandnames">>, _Actions) ->
	Pid = global:whereis_name(server_config),
	gen_server:call(Pid, {fetchclientidsandnames}, infinity);

process_item(<<"fetch">>, _Actions) ->
	Pid = global:whereis_name(server_config),
	gen_server:call(Pid, {fetch}, infinity);

process_item(<<"add">>, Actions) ->
	Parts = string:split(Actions, ",", all),
	Result = validate_add_command(Actions, Parts, #{}),
	case Result of
		{ok, Map} ->
			IdKey = maps:get(<<"client_id">>, Map),
			NameKey = binary_to_atom(maps:get(<<"name">>, Map)), 
			ChannelID = maps:get(<<"channel_id">>, Map),

			Test = maps:is_key(?YOUTUBE_KEY, Map),
			if
				Test =:= true ->
					NewRecord = #{IdKey => {NameKey, [{youtubekey, maps:get(?YOUTUBE_KEY, Map)}, {channel_id, ChannelID} ] }},
					gen_server:call(global:whereis_name(server_config), {addconfigrecord, NewRecord}, infinity);

				true ->
					NewRecord = #{IdKey => {NameKey, [{youtubekey, server_config_processor:get_default_youtube_key("server_config.cfg")}, {channel_id, ChannelID} ] }},
					gen_server:call(global:whereis_name(server_config), {addconfigrecord, NewRecord}, infinity)
			end;

		{error, Message} ->
			{error, #{error => Message}}
	end;
process_item(InvalidCommand, _Arg) ->
	{error, Message} = format_error(<<"invalid command: ">>, InvalidCommand),
	{error, #{error => Message}}.

	
validate_commands([], _Arg2, Map) ->
	{ok, Map};
validate_commands([Command|Tail], CommandList, Map) ->
	case lists:member(Command, CommandList) of
		true ->
			validate_commands(Tail, CommandList, Map);
		false ->
			format_error(<<"Missing parameter: ">>, Command)
	end.

validate_add_command(_Arg1, [], Map) -> 
	CommandList = [<<"client_id">>, <<"name">>, <<"channel_id">>],
	validate_commands(CommandList, maps:keys(Map), Map);

validate_add_command(Actions, [Head|Tail], Map) ->
	Items = string:split(Head, <<"=">>),
	Label = lists:nth(1, Items),
	if
		length(Items) < 2 ->
			format_error(<<"Malformed command: ">>, Head);
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

format_error(ErrorType, ErrorMessage) ->
	{error, <<ErrorType/binary, ErrorMessage/binary>>}.

