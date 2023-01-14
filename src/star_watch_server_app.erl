-module(star_watch_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("include/apod_record_def.hrl").
-include("include/apodtelemetry.hrl").
-include("include/macro_definitions.hrl").
-include("include/celestial_object_table.hrl").

start(_Type, _Args) ->
    initialize_mnesia(), %% Start mnesia
    % ppool_supersup:start_link(),  %% Start the pool manager
    % ppool:start_pool(database_server, 10, {database_server, start_link, []}), %% Create a pool of db workers
    
    ApiKeyConstraints   = { api_key,    [fun validate_access_key/2] },
    APODDateConstraints     = {start_date,  [fun validate_date_apod/2]},
    YearStartConstraint = {year_start, [fun validate_year_start/2]},
    YearEndConstraint = {year_end, [fun validate_year_end/2]},
        
    FetchNasaImagesRoute = {"/astronomy/celestialbody/[...]", [ApiKeyConstraints, YearStartConstraint, YearEndConstraint], celestial_body_handler, []},
    FetchApodRoute = {"/astronomy/apod/[...]", [ApiKeyConstraints, APODDateConstraints], star_watch_handler, []},
    RegistrationRoute = {"/telemetry/request/[...]", [ApiKeyConstraints], telemetry_request_handler, []},
    StatsRoute = {"/telemetry/stats/[...]", [ApiKeyConstraints], telemetry_handler, []},
    CatchAllRoute = {"/[...]", no_such_endpoint, []},
    Dispatch = cowboy_router:compile([
        {'_', [FetchNasaImagesRoute, FetchApodRoute, StatsRoute, RegistrationRoute, CatchAllRoute]}
    ]),
    {ok, _} = cowboy:start_clear(star_watch_http_listener,
         [{port,8083}],
        #{env => #{dispatch => Dispatch}}
    ),
    inets:start(),
    utils:start_cron_job(),
    star_watch_apod_sup:start_link().

stop(_State) ->
	ok.

validate_year_start(forward, Date) ->
    IntDate = binary_to_integer(Date),
    {ok, IntDate}.

validate_year_end(forward, Date) ->
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

initialize_mnesia() -> 
    mnesia:start(),
    mnesia:wait_for_tables([apodimagetable], 5000),
    case mnesia:table_info(apodimagetable, size) of
        0 -> 
            io:format("Initializing empty db~n"),
            mnesia:stop(),
%            application:set_env(mnesia, dir, "/tmp/star_watch_db"),
            mnesia:create_schema([node()]),
            mnesia:start(),
            create_table(apodimagetable),
            create_table(apodtelemetry),
            create_table(celestial_object_table),
            mnesia:wait_for_tables([apodimagetable, apodtelemetry, celestial_object_table], 5000);
        _ -> 
            io:format("DB already initialized~n"),
            ok
    end.

create_table(apodtelemetry) ->
    mnesia:create_table(
        apodtelemetry,
        [
            {attributes, record_info(fields, apodtelemetry)},
            {type, ordered_set},
            {disc_copies, [node()]}
        ]);
create_table(apodimagetable) ->
    mnesia:create_table(
        apodimagetable,
        [
            {attributes, record_info(fields, apodimagetable)},
            {index, [#apodimagetable.date, #apodimagetable.title, #apodimagetable.hdurl]}, 
            {type, ordered_set},
            {disc_copies, [node()]}
        ]);
create_table(celestial_object_table) ->
    mnesia:create_table(
        celestial_object_table,
        [
            {attributes, record_info(fields, celestial_object_table)},
            {index, [#celestial_object_table.key, #celestial_object_table.date]}, 
            {type, ordered_set},
            {disc_copies, [node()]}
        ]).
