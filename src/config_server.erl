% @Author: Oleg Zilberman
% @Date:   2023-03-11 18:33:02
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-27 14:33:30
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
	ProfileMap = server_config_processor:fetch_profile_map_from_db(),
    FetchResult = utils:jsonify_client_profile_table(ProfileMap),
	{reply, {ok, FetchResult}, State};

handle_call({fetchclientconfigdata, ClientID}, _From, State) ->
    FetchResult = server_config_processor:fetch_client_config_data(json, ClientID),
    {reply, FetchResult,  State};

handle_call({addconfigrecord, ClientID, Name, YoutubeKey, ChannelID}, _From, State) ->
    FetchResult = server_config_processor:add_client_pending_config_data(ClientID, Name, YoutubeKey, ChannelID),
    {reply, FetchResult,  State};

handle_call({promoteconfigrecord, ClientID}, _From, State) ->
    FetchResult = server_config_processor:promote_client_pending_config_data(ClientID),
    {reply, FetchResult,  State};

handle_call({deleteconfigrecord, ClientID}, _From, State) ->
    FetchResult = server_config_processor:fetch_client_config_data(json, ClientID),
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