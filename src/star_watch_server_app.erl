-module(star_watch_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("include/apod_record_def.hrl").
-include("include/apodtelemetry.hrl").
-define(API_KEY, <<"K9jqPfqphwz3s1BsTbPQjsi2c4kn4eV7wBFh2MR8">>).

start(_Type, _Args) ->
    ApiKeyConstraints = { api_key, [fun validate_access_key/2] },
    DateConstraints = {start_date, [fun validate_date/2]},
        
    FetchApodRoute = {"/astronomy/apod/[...]", [ApiKeyConstraints, DateConstraints], star_watch_handler, []},
    StatsRoute = {"/telemetry/stats/[...]", [ApiKeyConstraints], stats_handler, []},
    RegistrationRoute = {"/telemetry/register[...]", [ApiKeyConstraints], telemetry_handler, []},
    CatchAllRoute = {"/[...]", no_such_endpoint, []},
    Dispatch = cowboy_router:compile([
        {'_', [StatsRoute, RegistrationRoute, FetchApodRoute, CatchAllRoute]}
    ]),
    {ok, _} = cowboy:start_clear(star_watch_http_listener,
         [{port,80}],
        #{env => #{dispatch => Dispatch}}
    ),
    inets:start(),
    utils:start_cron_job(),
    star_watch_server_sup:start_link().

stop(_State) ->
	ok.

validate_date(forward, Date) ->
    DateLength = length(string:split(Date, "-", all)),
    if is_binary(Date) =:= false -> {error, bad_start_date};
       DateLength =/= 3          -> {error, bad_start_date};
       true                      -> {ok, Date}
    end.

validate_access_key(forward, Value) when Value =:= ?API_KEY ->
    {ok, Value};
validate_access_key(forward, _) ->
    {error, bad_api_key}.