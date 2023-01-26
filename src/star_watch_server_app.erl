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
    FetchApodRoute = {"/astronomy/apod/[...]", [ApiKeyConstraints, APODDateConstraints], star_watch_handler, [1]},
    RegistrationRoute = {"/telemetry/request/[...]", [ApiKeyConstraints], telemetry_request_handler, []},
    TelemetryRoute = {"/telemetry/stats/[...]", [ApiKeyConstraints], telemetry_handler, []},
    CatchAllRoute = {"/[...]", no_such_endpoint, []},
    Dispatch = cowboy_router:compile([
        {'_', [FetchNasaImagesRoute, FetchApodRoute, TelemetryRoute, RegistrationRoute, CatchAllRoute]}
    ]),
    {ok, _} = cowboy:start_clear(star_watch_http_listener,
         [{port,8083}],
        #{env => #{dispatch => Dispatch}}
    ),
    inets:start(),
    utils:start_cron_job(),
    star_watch_master_sup:start_link().

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
    mnesia:stop(),
    mnesia:create_schema([node() | nodes()]),
    mnesia:start(),
    init_table(apodimagetable),
    init_table(apodtelemetry),
    init_table(celestial_object_table),
    mnesia:wait_for_tables([apodimagetable, apodtelemetry, celestial_object_table], 5000).

init_table(TableName) ->
    case mnesia:table_info(TableName, size) of
        0 ->
            io:format("Initializing empty table: ~p~n", [TableName]),
            create_table(TableName);
        InitData ->
            io:format("DB Table: ~p exists with size: ~p~n", [TableName, InitData]),
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
            {type, bag},
            {disc_copies, [node()]}
        ]).
