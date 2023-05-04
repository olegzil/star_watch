% @Author: Oleg Zilberman
% @Date:   2023-03-21 11:05:14
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-30 17:21:40
-module(db_access_server).
-behaviour(gen_server).
-include("include/macro_definitions.hrl").
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).
-compile(export_all).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init([{FileName}]) ->
    {ok, FileName}.
   

%%% OTP Callbacks
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({fetchclientdirectory, ClientID}, _From, State) ->
    _FileName = State, 
    {Code, Data} = server_config_processor:fetch_client_directory(ClientID),
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

handle_call({linkstatus, ClientID, VideoLink}, _From, State) ->
    {ok, Code} = db_access:get_link_status(ClientID, VideoLink),
    Result = lists:keyfind(Code, 1, ?RESPONSE_CODES),
    RequestResult = 
    if
        Result =:= false ->
            BinaryCode = atom_to_binary(Code),
            throw(<<"no such code: ", BinaryCode/binary>>);
        true ->
            {_AtomicCode, ServerErrorCode} = Result,
            {ok , Return} = utils:format_success(ServerErrorCode, VideoLink),
            {ok, jiffy:encode(#{success => Return})}
    end,
    {reply, RequestResult, State};

handle_call({addvideolink, ClientID, VideoLink}, _From, State) ->
    RequestResult = db_access:add_video_link(ClientID, VideoLink),
    Response = case RequestResult of
        {error, Code} ->
            Result = lists:keyfind(Code, 1, ?RESPONSE_CODES),
            if
                Result =:= false ->
                    {error , Return} = utils:format_error(Code, Code),
                    {error, jiffy:encode(#{error => Return})};
                true ->
                    {AtomicCode, ServerErrorCode} = Result,
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

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Private Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fetch_channel_data(serverfirst, ClientID, ChannelID) ->
        respond_to_video_fetch_request(ClientID, ChannelID, []);

fetch_channel_data(dbfirst, ClientID, ChannelID) ->
        ListOfRecords = db_access:fetch_videos_for_channel_id(ChannelID),
        respond_to_video_fetch_request(ClientID, ChannelID, ListOfRecords).


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
            {_, ListOfRecords} = db_access:fetch_videos_for_channel_id(ChannelID),
            utils:package_channel_record_list(ListOfRecords)      
    end;
respond_to_video_fetch_request(_ClientID, _ChannelID, ListOfRecords) ->
    utils:package_channel_record_list(ListOfRecords).      

    


    