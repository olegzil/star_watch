% @Author: Oleg Zilberman
% @Date:   2023-01-11 19:37:07
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-20 16:45:50
-module(nasa_data_aquisition_server).
-behaviour(gen_server).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).
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
    fetch_next_celestial_object(CelestialObject, YearStart, YearEnd),
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
%%% This function takes a list of objects [jupiter, mars, ... etc] and calls 
%%% process_next page on each of them until the list is exhausted.
fetch_next_celestial_object([CelestialObject|Tail], YearStart, YearEnd) ->
    process_page(loop, CelestialObject, YearStart, YearEnd, 1),
    fetch_next_celestial_object(Tail, YearStart, YearEnd);

fetch_next_celestial_object([], _YearStart, _YearEnd) -> ok.

%%% This function makes a http call to get a list of root pages based on celestial object name (jupiter, mars, etc) and a page count.
%%% For each successful return, the json data is decoded, the 'collection' maps is extracted and the page is processed via process_page.
%%% For error return, the entire json package is ignored. 
%%% This process continues by incrementing the page count. Once the server returns an empty list of 'items' values, the function terminates.
process_page(loop, CelestialObject, YearStart, YearEnd, Page) ->
    case nasa_rest_access:fetch_root_page(CelestialObject, YearStart, YearEnd, Page) of
        {ok, Data} ->
            DecodedData = jiffy:decode(Data, [return_maps]),
            DataMap = maps:get(<<"collection">>, DecodedData),
            Size = erlang:length(maps:get(<<"items">>, DataMap)),
            io:format("items length: ~p~n", [Size]),
            case Size of
                 0 ->
                    process_page(exitloop, CelestialObject, YearStart, YearEnd, 1);
                 _ ->
                    nasa_rest_access:parse_nasa_data_update_db(CelestialObject, DecodedData),
                    process_page(loop, CelestialObject, YearStart, YearEnd, Page+1)
            end;
        {error, Error} ->
            io:format("Error retriving data: ~p~n", [Error]),
            process_page(exitloop, CelestialObject, YearStart, YearEnd, 0)
    end;

process_page(exitloop, _CelestialObject, _YearStart, _YearEnd, _Page) -> ok.