% @Author: Oleg Zilberman
% @Date:   2023-01-11 19:37:07
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-23 14:06:08
-module(nasa_data_aquisition_server).
-behaviour(gen_server).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2, async_call/4]).
-include("include/macro_definitions.hrl").
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init({CelestialObject, StartDate, EndDate}) ->
    {ok, {CelestialObject, StartDate, EndDate}}.

%%% OTP Callbacks
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({fetchnasadata}, _From, State) ->
	{CelestialObject, YearStart, YearEnd} = State,
    spawn(?MODULE, async_call, [YearStart, YearEnd, 1, CelestialObject]),
    {reply, {ok, <<"success">>}, State};


handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%% Private functions %%%%%%%%%%%%%%%%%%%%%%%%%%
async_call(YearStart, YearEnd, Page, CelestialObject) ->
    fetch_next_celestial_object(YearStart, YearEnd, Page, CelestialObject).    

%%% This function takes a list of objects [jupiter, mars, ... etc] and calls 
%%% process_next page on each of them until the list is exhausted.
fetch_next_celestial_object(YearStart, YearEnd, Page, [CelestialObject|Tail]) ->
    io:format("processing page #~p~n", [Page]),
    process_page(loop, CelestialObject, YearStart, YearEnd, Page),
    fetch_next_celestial_object(YearStart, YearEnd, Page, Tail);

fetch_next_celestial_object(_YearStart, _YearEnd, _Page, []) -> ok.

%%% This function makes a http call to get a list of root pages based on celestial object name (jupiter, mars, etc) and a page count.
%%% For each successful return, the json data is decoded, the 'collection' maps is extracted and the page is processed via process_page.
%%% For error return, the entire json package is ignored. 
%%% This process continues by incrementing the page count. Once the server returns an empty list of 'items' values, the function terminates.
process_page(loop, CelestialObject, YearStart, YearEnd, Page) ->
    {RetCode, Data} = nasa_rest_access:fetch_root_page(CelestialObject, YearStart, YearEnd, Page),
    if 
        RetCode =:= ok ->
            DecodedData = jiffy:decode(Data, [return_maps]),
            DataMap = maps:get(<<"collection">>, DecodedData),
            Items = maps:get(?YOUTUBE_VIDEO_ARRAY_KEY, DataMap),
            if 
                Items =/= [] ->
                    nasa_rest_access:parse_nasa_data_update_db(CelestialObject, DecodedData),
                    process_page(loop, CelestialObject, YearStart, YearEnd, Page+1);
                true -> 
                    io:format("Last page processed #~p~n", [Page]),
                    ok
            end;
        true ->
            io:format("Error retriving data: ~p~n", [Data])
    end.