% @Author: Oleg Zilberman
% @Date:   2023-03-21 11:05:14
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-21 11:18:31
-module(db_access_server).
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

handle_call({fetchchannelvideos, ClientKey}, _From, State) ->
	FileName = State,
    RequestResult = db_access:process_channel_request(FileName, ClientKey),
	{reply, RequestResult, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.