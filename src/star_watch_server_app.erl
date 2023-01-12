-module(star_watch_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("include/apod_record_def.hrl").
-include("include/apodtelemetry.hrl").
-include("include/macro_definitions.hrl").

start(_Type, _Args) ->
    ApiKeyConstraints   = { api_key,    [fun validate_access_key/2] },
    APODDateConstraints     = {start_date,  [fun validate_date_apod/2]},
    NASADateConstraints = {year_start, [fun validate_date_nasa/2]},
        
    FetchNasaImagesRoute = {"/astronomy/celestialbody/[...]", [ApiKeyConstraints, APODDateConstraints], celestial_body_handler, []}, %%%TODO: Create celestial_body_handler
    FetchApodRoute = {"/astronomy/apod/[...]", [ApiKeyConstraints, APODDateConstraints], star_watch_handler, []},
    RegistrationRoute = {"/telemetry/request/[...]", [ApiKeyConstraints], telemetry_request_handler, []},
    StatsRoute = {"/telemetry/stats/[...]", [ApiKeyConstraints], telemetry_handler, []},
    CatchAllRoute = {"/[...]", no_such_endpoint, []},
    Dispatch = cowboy_router:compile([
        {'_', [FetchApodRoute, StatsRoute, RegistrationRoute, CatchAllRoute]}
    ]),
    {ok, _} = cowboy:start_clear(star_watch_http_listener,
         [{port,8082}],
        #{env => #{dispatch => Dispatch}}
    ),
    inets:start(),
    utils:start_cron_job(),
    star_watch_server_sup:start_link().

stop(_State) ->
	ok.

validate_date_nasa(forward, Date) ->
    IntDate = binary_to_integer(Date),
    {ok, IntDate}.

validate_date_apod(forward, Date) ->
    DateLength = length(string:split(Date, "-", all)),
    if is_binary(Date) =:= false -> {error, bad_start_date};
       DateLength =/= 3          -> {error, bad_start_date};
       true                      -> {ok, Date}
    end.


validate_access_key(forward, Value) when Value =:= ?API_KEY ->
    {ok, Value};
validate_access_key(forward, _) ->
    {error, bad_api_key}.