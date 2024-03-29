% @Author: Oleg Zilberman
% @Date:   2023-02-23 17:35:41
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-05 17:19:46
-module(apod_data_aquisition).
-export([fetch_apod_data/3,fetch_data/1]).
-include("include/macro_definitions.hrl").

fetch_data(now) ->
	Future = 0,
	Past = 0,
	fetch_apod_data(production, Past, Future);	

fetch_data(periodic) ->
	Past = 24*60*60,
	Future = 0,
	fetch_apod_data(production, Past, Future).

fetch_apod_data(production, Past, Future) ->
	Query = uri_string:compose_query([{"start_date", utils:time_pair_to_fetch(past, Past)}, 
									  {"end_date", utils:time_pair_to_fetch(future, Future)},
									  {"api_key", ?ASTRONOMY_API_KEY},
									  {"thumbs", "true"}
									  ]),
	Request = string:join([?APOD_HOST, Query], ""),
    Options = [{ssl, [{verify, verify_none}]}],
	case httpc:request(get, {Request, []}, Options, []) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			utils:update_database(apod, Body);
		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end;

fetch_apod_data(notfound, GregorianStartDays, GregorianEndDays) ->
	S = calendar:gregorian_days_to_date(GregorianStartDays),
	E = calendar:gregorian_days_to_date(GregorianEndDays),
	fetch_apod_data(tuples, S, E);

fetch_apod_data(tuples, {StartYear, StartMonth, StartDay}, {EndYear, EndMonth, EndDay}) ->
	Past = binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p-~.2.0p-~.2.0p",[StartYear, StartMonth, StartDay]))),
	Future = binary:bin_to_list(list_to_binary(io_lib:format("~.4.0p-~.2.0p-~.2.0p",[EndYear, EndMonth, EndDay]))),
	Query = uri_string:compose_query([{"start_date", Past}, 
									  {"end_date", Future},
									  {"api_key", ?ASTRONOMY_API_KEY},
									  {"thumbs", "true"}
									  ]),
	Request = string:join([?APOD_HOST, Query], ""),
    Options = [{ssl, [{verify, verify_none}]}],
	case httpc:request(get, {Request, []}, Options, []) of
		{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
			utils:update_database(apod, Body),
			{ok, Body};
		{ok,{_,_,ErrorMessage}} ->
			{error, ErrorMessage};
		Other ->
			{error, Other}
	end.
