% @Author: Oleg Zilberman
% @Date:   2022-10-20 11:39:41
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-11-17 11:00:22
-module(database_server).
-behaviour(gen_server).
-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).
start_link(StartDate, EndDate) ->
    gen_server:start_link(?MODULE, {StartDate, EndDate} , []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init({StartDate, EndDate}) ->
    {ok, {StartDate, EndDate}}.

%%% OTP Callbacks
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({fetchdata}, _From, State) ->
	{Start, End} = State,
	FetchResult = db_access:process_date_request(Start, End),
	{reply, FetchResult, State};

handle_call({datasetsize}, _From, State) ->
    {Start, End} = State,
    FetchResult = db_access:get_dataset_size(Start, End),
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