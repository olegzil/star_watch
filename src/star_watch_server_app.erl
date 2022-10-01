-module(star_watch_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/test/[...]", star_watch_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(star_watch_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    star_watch_server_sup:start_link().

stop(_State) ->
	ok.
