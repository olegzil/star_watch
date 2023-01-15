% @Author: Oleg Zilberman
% @Date:   2023-01-13 16:45:38
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-15 10:44:12

-module(star_watch_master_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).
-export([start_link/0]).
-export([init/1, attach_child/2]).
-include("include/macro_definitions.hrl").
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 3600}, []}}.
attach_child(Child, Args) when Child =:= apod ->
                    ApodServer = {apod_server, 
                            {database_server, start_link, [Args]}, 
                            transient, 5000, worker, [database_server]},
                    {ok, Response = supervisor:start_child(?MODULE, ApodServer)};
attach_child(_Child, _Args) ->
    {error, invalid_child_requested}.




