% @Author: Oleg Zilberman
% @Date:   2023-01-11 19:37:07
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-14 23:09:11
-module(nasa_data_aquisition_server).
-behaviour(gen_server).
-export([start_link/3, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).
start_link(CelestialObject, StartDate, EndDate) ->
    gen_server:start_link(?MODULE, {CelestialObject, StartDate, EndDate} , []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init({CelestialObject, StartDate, EndDate}) ->
    {ok, {StartDate, EndDate}}.

%%% OTP Callbacks
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({fetchnasadata}, _From, State) ->
	{CelestialObject, YearStart, YearEnd} = State,
	FetchResult = nasa_rest_access:fetch_root_page(CelestialObject, YearStart, YearEnd),
	{reply, FetchResult, State};


handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.