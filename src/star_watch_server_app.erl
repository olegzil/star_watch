-module(star_watch_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-include("include/apod_record_def.hrl").
start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/test/[...]", star_watch_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(star_watch_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    initialize_mnesia(),
    star_watch_server_sup:start_link().

stop(_State) ->
	ok.
initialize_mnesia() -> 
    try mnesia:create_schema([node()]) of
        {error, _} -> io:format("schema exists~n")
    after
        mnesia:start(),
        CreateResult = mnesia:create_table(
            apodimagetable,
            [
                {attributes, record_info(fields, apodimagetable)},
                {index, [#apodimagetable.date]}, 
                {type, ordered_set},
                {disc_copies, nodes()}
            ]),
        io:format("create table result = ~p~n", [CreateResult])
    end.


