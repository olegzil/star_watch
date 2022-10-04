% @Author: oleg
% @Date:   2022-09-27 14:59:44
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-10-04 16:17:26

-module(db_access).
-export([insert_apod_entries/1, update_db_from_json_file/1, readlines/1]).
-export([dump_db/0]).
-include ("include/apod_record_def.hrl").
%%
%% This function populates a mnesia database with the contests of all
%% files found in [DirName]. Files are assumed to be valid json.
%%
update_db_from_json_file(DirName) ->
	{ok, Filelist} = file:list_dir(DirName),	%generate a list of file names from the directory provided
	process_file_list(DirName, Filelist).		%parse the files and populate DB


%% This function reads a list of files recusively, generates a single list containing
%% all parsed data and calls jiffy to convert the data from string to Json.
%% [DirName] -- the name of the directory. It will be concatinated with the file name
%% [FileName | T] -- a pattern matched list of file names

process_file_list(DirName, [FileName|T]) ->
    FileData = readlines(filename:join(DirName, FileName)),  %% read all data from the file 
    try jiffy:decode(FileData, []) of
    	JsonData ->
		    insert_apod_entries(JsonData)					 %% insert the json data into mnesia
    catch
    	Class:Reason ->
    		io:format("~p~n ~p~n", [Reason, filename:join(DirName, FileName)])
    after
	    process_file_list(DirName, T)							 %% do it again recurcively			
    end;

%% Terminating call for the tail recurcive call above. 
process_file_list(_, []) ->
    true.

%% This function inserts [JsonData] into a mnesia database
%% [JsonData] -- well formed json data
insert_apod_entries(JsonData) when JsonData =/= [] ->
	Fun = fun() ->	% The function used in a mnesia transaction
		lists:foreach(	%% for each item in the list
		fun({ApodEntry}) ->
			Record = from_string_to_json_apod(ApodEntry), %% convert the item to a struct
			case json_key_filter(Record) of 			%% for now we only store image records
				true ->
					mnesia:write(Record);				%% if this is an image, store it
				false ->	%% if this is not an image, print message to terminal
					io:format("Ignoring video: ~p~n", [Record#apodimagetable.url])
			end
		end,
		JsonData)
	end,
	mnesia:transaction(Fun). %% execute the transaction

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Private Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try 
    	get_all_lines(Device)
      after 
      	file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.

from_string_to_json_apod(Item) ->
	V = #apodimagetable{
				url 			= proplists:get_value(<<"url">>, Item),
				copyright 		= proplists:get_value(<<"copyright">>, Item, "none"),
				date 			= proplists:get_value(<<"date">>, Item),
				explanation		=	proplists:get_value(<<"explanation">>, Item),
				hdurl			=	proplists:get_value(<<"hdurl">>, Item),
				media_type		=	proplists:get_value(<<"media_type">>, Item),
				service_version	=	proplists:get_value(<<"service_version">>, Item),
				title 			=	proplists:get_value(<<"title">>, Item)
				}.

json_key_filter(Record) ->
	if 
		Record#apodimagetable.media_type =:= <<"image">> -> true;
		true -> false
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Debug functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dump_db() ->
mnesia:activity(sync_dirty,
	Fun = fun() ->
	  mnesia:foldl(
	      fun(#apodimagetable{}, Acc) ->
	          %% io:format("Record --> ~p~s", [Acc]),
	          Acc
	      end,
	      ignored_acc,
	      apodimagetable)
	end),
	mnesia:transaction(Fun). %% execute the transaction

