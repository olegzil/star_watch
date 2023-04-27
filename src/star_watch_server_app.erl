-module(star_watch_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("include/apod_record_def.hrl").
-include("include/apodtelemetry.hrl").
-include("include/youtube_channel.hrl").
-include("include/macro_definitions.hrl").
-include("include/celestial_object_table.hrl").
-include("include/client_profile_table.hrl").
-include("include/server_config_item.hrl").
 % cowboy_req:match_qs([{action, nonempty}, {client_key, [], ?CLIENT_ACCESS_KEY}], Request) of

start(_Type, _Args) ->
    initialize_mnesia(), %% Start mnesia
    ApiKeyConstraints           = {api_key,    [fun validate_access_key/2]},
    APODDateConstraints         = {start_date,  [fun validate_date_apod/2]},
    YearStartConstraint         = {year_start, [fun validate_year_start/2]},
    YearEndConstraint           = {year_end, [fun validate_year_end/2]},
        
    FetchAdminRoute = {"/youtube/admin/[...]", [], youtube_admin_channel_handler, []},
    FetchYoutubeChanneRoute = {"/youtube/channelselector/[...]", [], youtube_channel_directory_handler, []},
    FetchNasaImagesRoute = {"/astronomy/celestialbody/[...]", [ApiKeyConstraints, YearStartConstraint, YearEndConstraint], celestial_body_handler, []},
    FetchApodRoute = {"/astronomy/apod/[...]", [ApiKeyConstraints, APODDateConstraints], star_watch_handler, [1]},
    RegistrationRoute = {"/telemetry/request/[...]", [ApiKeyConstraints], telemetry_request_handler, []},
    TelemetryRoute = {"/telemetry/stats/[...]", [ApiKeyConstraints], telemetry_handler, []},
    CatchAllRoute = {"/[...]", no_such_endpoint, []},
    Dispatch = cowboy_router:compile([
        {'_', [FetchAdminRoute, FetchYoutubeChanneRoute, FetchNasaImagesRoute, FetchApodRoute, TelemetryRoute, RegistrationRoute, CatchAllRoute]}
    ]),
    {ok, _} = cowboy:start_clear(star_watch_http_listener,
         [{port,8083}],
        #{env => #{dispatch => Dispatch}}
    ),
    inets:start(),
    % utils:start_cron_job(youtube),
    utils:start_cron_job(apod),
    MasterPid = star_watch_master_sup:start_link(),
    %% Create a child process that will handle all file access to the server_config.cfg file.
    {_, {_, DbServerPid}} = star_watch_master_sup:attach_child(db_access_server, {"server_config.cfg"}),
    {_, {_, ServerConfigPid}} = star_watch_master_sup:attach_child(serverconfig, {"server_config.cfg"}),
    if
        is_pid(ServerConfigPid) andalso is_pid(DbServerPid) ->
            MasterPid;
        true ->
            {error, failure_creating_child_process} %% Broke ass server
    end.

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


validate_access_key(forward, Value) when Value =:= ?ASTRONOMY_API_KEY ->
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
    init_table(youtube_channel),
    init_table(client_profile_table_pending),
    Empty = init_table(client_profile_table),
    mnesia:wait_for_tables([apodimagetable, apodtelemetry, celestial_object_table, youtube_channel, client_profile_table, client_profile_table_pending], 10000),
    timer:apply_after(1000, server_config_processor, populate_client_profile_table, [Empty]),
    mnesia:add_table_index(youtube_channel, video_id).

init_table(TableName) ->
    case mnesia:table_info(TableName, size) of
        0 ->
            io:format("Initializing empty table: ~p~n", [TableName]),
            create_table(TableName),
            true;
        undefined ->
            io:format("Initializing empty table: ~p~n", [TableName]),
            create_table(TableName),
            true;

        InitData ->
            io:format("DB Table: ~p exists with size: ~p~n", [TableName, InitData]),
            false
    end.

create_table(client_profile_table_pending) ->
    mnesia:create_table(
        client_profile_table_pending,
        [
            {attributes, record_info(fields, client_profile_table_pending)},
            {type, ordered_set},
            {disc_copies, [node()]}
        ]);
    
create_table(client_profile_table) ->
    mnesia:create_table(
        client_profile_table,
        [
            {attributes, record_info(fields, client_profile_table)},
            {type, ordered_set},
            {disc_copies, [node()]}
        ]);

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
create_table(youtube_channel) ->
    mnesia:create_table(
        youtube_channel,
        [
            {attributes, record_info(fields, youtube_channel)},
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

