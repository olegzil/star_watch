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
        StartResult = mnesia:start(),
        mnesia:change_table_copy_type(schema, node(), disc_copies),
        CreateResult = mnesia:create_table(apodimagetable, [{disc_copies, [node()]}, {type, ordered_set}, {attributes, record_info(fields, apodimagetable)}]),
        io:format("add table ~p~n", [mnesia:add_table_index(apodimagetable, 3)]),
        io:format("StartResult = ~p~nCreateResult = ~p~n", [StartResult, CreateResult])
    end.


