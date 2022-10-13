-module(star_watch_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-include("include/apod_record_def.hrl").
-define(API_KEY, <<"K9jqPfqphwz3s1BsTbPQjsi2c4kn4eV7wBFh2MR8">>).

start(_Type, _Args) ->
    ApiKeyConstraints = { api_key, [fun validate_access_key/2] },
    FetchApodRoute = {"/astronomy/[...]", [ApiKeyConstraints], star_watch_handler, []},
    CatchAllRoute = {"/[...]", no_such_endpoint, []},
    Dispatch = cowboy_router:compile([
        {'_', [FetchApodRoute, CatchAllRoute]}
    ]),
    {ok, _} = cowboy:start_clear(star_watch_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    initialize_mnesia(),
    inets:start(),
    utils:start_cron_job(),
    star_watch_server_sup:start_link().

stop(_State) ->
	ok.

validate_access_key(forward, Value) when Value =:= ?API_KEY ->
    {ok, Value};
validate_access_key(forward, _) ->
    {error, bad_api_key}.

initialize_mnesia() -> 
    case mnesia:table_info(apodimagetable, size) of
        0 -> 
            io:format("Initializing empty db~n"),
            mnesia:stop(),
            application:set_env(mnesia, dir, "/tmp/star_watch_db"),
            mnesia:create_schema([node()]),
            mnesia:start(),

            mnesia:create_table(
                apodimagetable,
                [
                    {attributes, record_info(fields, apodimagetable)},
                    {index, [#apodimagetable.date, #apodimagetable.title, #apodimagetable.hdurl]}, 
                    {type, ordered_set},
                    {disc_copies, [node()]}
                ]),
            mnesia:wait_for_tables([apodimagetable], 5000);
        _ -> 
            io:format("DB already initialized~n"),
            ok
    end.
