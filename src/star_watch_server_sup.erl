-module(star_watch_server_sup).
-behaviour(supervisor).
-include("include/apod_record_def.hrl").
-include("include/apodtelemetry.hrl").
-define(SERVER, ?MODULE).
-export([start_link/0]).
-export([init/1, create_table/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    initialize_mnesia(), %% Start mnesia
    % ppool_supersup:start_link(),  %% Start the pool manager
    % ppool:start_pool(database_server, 10, {database_server, start_link, []}), %% Create a pool of db workers
    MaxRestart = 1,
    MaxTime = 5,
    {ok, {{simple_one_for_one, MaxRestart, MaxTime},
          [{serv, 
             {database_server, start_link, []},
              temporary, 
              1000,
              worker,
              [simple_one_for_one]}
           ]
          }
    }.

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
	        mnesia:wait_for_tables([apodimagetable, apodtelemetry, mnesiatable], 5000);
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
        ]).

