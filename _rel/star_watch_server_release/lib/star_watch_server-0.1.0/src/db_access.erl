% @Author: oleg
% @Date:   2022-09-27 14:59:44
% @Last Modified by:   oleg
% @Last Modified time: 2022-09-30 17:14:49

-module(db_access).
-export([init_tables/0, insert_apod_entries/1, update_db_from_json_file/1, readlines/1]).

-record(apod_entry, {
		url,
		copyright,
		date,
		explanation,
		hdurl,
		media_type,
		service_version, 
		title 
	}).
init_tables() -> 
	mnesia:create_table(apod_entry, 
		[{attributes, record_info(fields, apod_entry)}]).

insert_apod_entries(ApodEntryList) ->
	Fun = fun() ->
			lists:foreach(
			fun(ApodEntry) ->
				mnesia:write(ApodEntry)
			end,
		ApodEntryList)
	end,
	mnesia:transaction(Fun).

update_db_from_json_file(Filename) ->
	FileData = readlines(Filename),
	jiffy:decode(FileData, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Private Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% populate_tables(JsonData) ->
% 	[ok, IoDevice] = file:open(JsonData, [read]).

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

