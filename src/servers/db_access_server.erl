% @Author: Oleg Zilberman
% @Date:   2023-03-21 11:05:14
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-30 17:21:40
-module(db_access_server).
-behaviour(gen_server).
-include("include/macro_definitions.hrl").
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2, fetch_channel_data/3]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init([{FileName}]) ->
    {ok, FileName}.
   

%%% OTP Callbacks
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({restoredefaultclient, ClientID}, _From, State) ->
    {Code, Data} = server_config_processor:restore_default_client(ClientID),
    case Code of
        ok ->
            {reply, {Code, Data},  State};
        _ ->
            {reply, {Code, jiffy:encode(Data)},  State}
    end;

handle_call({deletechannel, ClientID, ChannelID}, _From, State) ->
    {Code, Data} = db_access:delete_channel_from_client(ClientID, ChannelID),
    case Code of
        ok ->
            {reply, {Code, Data},  State};
        _ ->
            {reply, {Code, jiffy:encode(Data)},  State}
    end;

handle_call({fetchclientdirectory, ClientID}, _From, State) ->
    _FileName = State, 
    ValidClientID = db_access:get_valid_client_id(ClientID),
    {Code, Data} = server_config_processor:fetch_client_directory(ValidClientID),
    case Code of
        ok ->
            {reply, {Code, Data},  State};
        _ ->
            {reply, {Code, jiffy:encode(Data)},  State}
    end;

handle_call({fetchchannelvideos, ClientID, ChannelID}, _From, State) ->
    {Code, Data} = fetch_channel_data(dbfirst, ClientID, ChannelID),
    case Code of
        ok ->
            {reply, {Code, Data},  State};
        _ ->
            {reply, {Code, jiffy:encode(Data)},  State}
    end;

handle_call({updatechannel, ClientID, ChannelID}, _From, State) ->
    {Code, Data} = fetch_channel_data(serverfirst, ClientID, ChannelID),
    case Code of
        ok ->
            {reply, {Code, Data},  State};
        _ ->
            {reply, {Code, jiffy:encode(Data)},  State}
    end;

handle_call({addvideolink, ClientID, ChannelName, ChannelID, VideoID}, _From, State) ->
    RequestResult = server_config_processor:update_existing_client(ClientID, ChannelName, ChannelID, VideoID),
    Response = case RequestResult of
        {error, Code} ->
            Result = lists:keyfind(Code, 1, ?RESPONSE_CODES),
            if
                Result =:= false ->
                    {error , Return} = utils:format_error(Code, Code),
                    {error, jiffy:encode(#{error => Return})};
                true ->
                    {_AtomicCode, ServerErrorCode} = Result,
                    {error , Return} = utils:format_error(ServerErrorCode, VideoID),
                    {error, jiffy:encode(#{error => Return})}
            end;
        {ok, Code} ->
            Result = lists:keyfind(Code, 1, ?RESPONSE_CODES),
            if
                Result =:= false ->
                    {error , Return} = utils:format_success(Code, VideoID),
                    {ok, jiffy:encode(#{success => Return})};
                true ->
                    {_AtomicCode, ServerErrorCode} = Result,
                    {ok , Return} = utils:format_success(ServerErrorCode, VideoID),
                    {ok, jiffy:encode(#{success => Return})}
            end
    end,
    {reply, Response, State};

handle_call({deletevideolink, ClientID, VideoLink}, _From, State) ->
    RequestResult = db_access:delete_video_link_from_profile_table(ClientID, VideoLink),
    Response = case RequestResult of
        {error, Code} ->
            Result = lists:keyfind(Code, 1, ?RESPONSE_CODES),
            if
                Result =:= false ->
                    {error , Return} = utils:format_error(Code, Code),
                    {error, jiffy:encode(#{error => Return})};
                true ->
                    {_AtomicCode, ServerErrorCode} = Result,
                    {error , Return} = utils:format_error(ServerErrorCode, VideoLink),
                    {error, jiffy:encode(#{error => Return})}
            end;
        {ok, Code} ->
            Result = lists:keyfind(Code, 1, ?RESPONSE_CODES),
            if
                Result =:= false ->
                    {error , Return} = utils:format_success(Code, VideoLink),
                    {ok, jiffy:encode(#{success => Return})};
                true ->
                    {_AtomicCode, ServerErrorCode} = Result,
                    {ok , Return} = utils:format_success(ServerErrorCode, VideoLink),
                    {ok, jiffy:encode(#{success => Return})}
            end
    end,
    {reply, Response, State};

handle_call({linkstatus, ClientID, VideoLink}, _From, State) ->
    Found = db_access:is_video_in_youtube_channel(ClientID, VideoLink),
    Response = case Found of 
                    false ->
                        {ok, Result} = utils:format_success(?SERVER_ERROR_LINK_NOT_FOUND, VideoLink),
                        {ok, jiffy:encode(#{success => Result})};
                    true ->
                        {ok, Result} = utils:format_success(?SERVER_ERROR_LINK_EXISTS, VideoLink),
                        {ok, jiffy:encode(#{success => Result})}
                end,
    {reply, Response, State};

handle_call({fetch_video_data, ClientID, VideoLink}, _From, State) ->
    Found = db_access:is_video_in_youtube_channel(ClientID, VideoLink),
    Response = case Found of 
                    false ->
                        {ok, Result} = utils:format_success(?SERVER_ERROR_LINK_NOT_FOUND, VideoLink),
                        {ok, jiffy:encode(#{error => Result})};
                    true ->
                        {ok, Result} = db_access:fetch_video_data(ClientID, VideoLink),
                        {ok, jiffy:encode(#{video_data => Result})}
                end,
    {reply, Response, State};


handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(refreshclietprofiles, State) ->
    DBCompareFunction = fun({ChannelName, ChannelID}, Acc) ->
        case db_access:is_channel_in_db(ChannelID) of
            false ->
                Acc ++ [{ChannelName, ChannelID}];
            true ->
                Acc
        end
    end,
    MapFunction = fun(ClientProfile, Acc) ->
        ChannelList = maps:get(client_channel_data, ClientProfile),
        lists:foldl(DBCompareFunction, Acc, ChannelList)
    end,

    ProfileList = server_config_processor:fetch_profile_map_from_file(?SERVER_CONFIG_FILE),
    case lists:foldl(MapFunction, [], ProfileList) of 
        [] ->
            io:format("Not refreshing db. No new channels added~n");
        ChannelList ->
            UpdateFunction = fun({_Name, ID}) -> youtube_data_aquisition:fetch_single_channel(server_config_processor:get_client_key(?SERVER_CONFIG_FILE), ID) end,
            lists:foreach(UpdateFunction, ChannelList),
            io:format("Successful update of Channels: ~p~n", [ChannelList])
    end,
    {noreply, State};    

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fetch_channel_data(serverfirst, ClientID, ChannelID) ->
        respond_to_video_fetch_request(ClientID, ChannelID, []);

fetch_channel_data(dbfirst, ClientID, ChannelID) ->
        ListOfRecords = db_access:fetch_videos_for_channel_id(ChannelID),
        respond_to_video_fetch_request(ClientID, ChannelID, ListOfRecords).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Private Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

respond_to_video_fetch_request(ClientID, ChannelID, []) ->
    ChannelDescriptor = 
    case db_access:get_channel_descriptors_for_client(ClientID) of
            {error, Reason} ->
                utils:format_error(?SERVER_ERROR_NO_RECORDS_FOUND, Reason);
            {atomic, []} ->
                utils:format_error(?SERVER_ERROR_MISSING_CLIENT, no_such_client);
            {ok, TupleList} ->
                ChannelFound = lists:keyfind(ChannelID, 2, TupleList),
                if
                    ChannelFound =:= false ->
                        utils:format_error(?SERVER_ERROR_MISSING_CHANNEL, no_such_channel);
                    true ->
                        {ok, ChannelFound}
                end
            
        end,
    case ChannelDescriptor of
        {error, Cause} ->
            utils:format_error(?SERVER_ERROR_DB_ERROR, Cause);
        {ok, {_ChannelName, ChannelID}} ->
            {ok, YoutubeKey} = db_access:get_client_youtube_key(ClientID),
            ClientProfile = [{YoutubeKey, ChannelID}],
            youtube_data_aquisition:fetch_data(production, ClientProfile, []),
            ListOfRecords = db_access:fetch_videos_for_channel_id(ChannelID),
            utils:package_channel_record_list(ListOfRecords)      
    end;
respond_to_video_fetch_request(_ClientID, _ChannelID, ListOfRecords) ->
    utils:package_channel_record_list(ListOfRecords).      

    


    