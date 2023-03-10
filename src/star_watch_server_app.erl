-module(star_watch_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("include/apod_record_def.hrl").
-include("include/apodtelemetry.hrl").
-include("include/youtube_channel.hrl").
-include("include/macro_definitions.hrl").
-include("include/celestial_object_table.hrl").

start(_Type, _Args) ->
    initialize_mnesia(), %% Start mnesia
    % ppool_supersup:start_link(),  %% Start the pool manager
    % ppool:start_pool(database_server, 10, {database_server, start_link, []}), %% Create a pool of db workers
    
    YoutubeAdminKeyConstraints = {key, [fun validate_admin_key/2]},
    YoutubeApiConstraints  = {api_key, [fun validate_client_api_key/2]},
    ApiKeyConstraints   = { api_key,    [fun validate_access_key/2] },
    APODDateConstraints     = {start_date,  [fun validate_date_apod/2]},
    YearStartConstraint = {year_start, [fun validate_year_start/2]},
    YearEndConstraint = {year_end, [fun validate_year_end/2]},
        
    FetchAdminRoute = {"/youtube/admin/[...]", [YoutubeAdminKeyConstraints], youtube_admin_channel_handler, []},
    FetchYoutubeChanneRoute = {"/youtube/channelselector/[...]", [YoutubeApiConstraints], youtube_channel_directory_handler, []},
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
    utils:start_cron_job(youtube),
    utils:start_cron_job(apod),
    star_watch_master_sup:start_link().

stop(_State) ->
	ok.

find_user_id([], _Target) -> false;

find_user_id([UserProfile | Tail], Target) ->
    {ClientID, _, _, _} = UserProfile,
    case ClientID =:= Target of
        true -> true;
        false -> find_user_id(Tail, Target)
    end.

validate_admin_key(forward, Value) when ?ADMINISTRATOR_KEY =:= Value ->
    {ok, Value};
validate_admin_key(forward, _) ->
    {error, bad_admin_key}.

validate_client_api_key(forward, Value) ->
    ChannelList = server_config_processor:fetch_list_of_client_ids_and_channel_ids(),
    case find_user_id(ChannelList, Value) of 
        true -> 
            io:format("validate_client_api_key Success: ~p~n", [Value]),
            {ok, Value};
        false -> 
            io:format("validate_client_api_key Failure bad_youtube_api_key"),
            {error, bad_youtube_api_key}
    end.

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
    mnesia:wait_for_tables([apodimagetable, apodtelemetry, celestial_object_table, youtube_channel], 5000).

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
