% @Author: Oleg Zilberman
% @Date:   2023-03-11 18:33:02
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-13 16:53:33
-module(config_server).
-behaviour(gen_server).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init([{FileName}]) ->
    {ok, FileName}.
   

%%% OTP Callbacks
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({fetchprofilemap}, _From, State) ->
	{_FileName} = State,
	FetchResult = server_config_processor:fetch_profile_map(),
	{reply, FetchResult, State};

handle_call({fetchclientconfigdata, ClientID}, _From, State) ->
	{_FileName} = State,
    FetchResult = server_config_processor:fetch_client_config_data(ClientID),
    {reply, FetchResult,  State};

handle_call({fetchlistofchannelidsandyoutubekeys}, _From, State) ->
	{_FileName} = State, 
    FetchResult = server_config_processor:fetch_list_of_channel_ids_and_youtube_keys(),
    {reply, FetchResult,  State};

handle_call({fetchlistofclientidsandchannelids}, _From, State) ->
	{_FileName} = State, 
    FetchResult = server_config_processor:fetch_list_of_client_ids_and_channel_ids(),
    {reply, FetchResult,  State};

handle_call({fetchclientidsandnames}, _From, State) ->
	{_FileName} = State, 
    FetchResult = server_config_processor:fetch_list_of_client_ids_and_channel_ids(),
    {reply, FetchResult,  State};

handle_call({addconfigrecord, Record}, _From, State) ->
	{FileName} = State, 
    FetchResult = server_config_processor:update_config_record(FileName, Record),
    {reply, FetchResult,  State};

handle_call({deleteconfigrecord, Record}, _From, State) ->
    {FileName} = State, 
    FetchResult = server_config_processor:delete_record(FileName, Record),
    {reply, FetchResult,  State};


handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.