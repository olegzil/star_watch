% @Author: Oleg Zilberman
% @Date:   2023-01-13 16:45:38
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-11 18:55:10

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

%%%%%%%%%%%%%%%%%%%%% Public functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
attach_child(Child, Args) when Child =:= serverconfig ->
                    ChildName = list_to_atom(erlang:ref_to_list(make_ref())),
                    Server = {ChildName, 
                            {config_server, start_link, [Args]}, 
                            temporary, 5000, worker, [config_server]},
                    Result = child_start_selector(Server),
                    {ChildName, Result};

attach_child(Child, Args) when Child =:= apod ->
                    ChildName = list_to_atom(erlang:ref_to_list(make_ref())),
                    Server = {ChildName, 
                            {database_server, start_link, [Args]}, 
                            temporary, 5000, worker, [database_server]},
                    Result = child_start_selector(Server),
                    {ChildName, Result};

attach_child(Child, Args) when Child =:= nasageneralapi ->
                    ChildName = list_to_atom(erlang:ref_to_list(make_ref())),
                    Server = {ChildName, 
                            {nasa_data_aquisition_server, start_link, [Args]}, 
                            transient, 5000, worker, [nasa_data_aquisition_server]},
                    Result = child_start_selector(Server),
                    {ChildName, Result};

attach_child(_Child, _Args) ->
    {error, invalid_child_requested}.

%%%%%%%%%%%%%%%%%%%%% Private functions %%%%%%%%%%%%%%%%%%%%%
child_start_selector(Server) -> 
    {ServerID, _, _, _, _, _} =  Server,
    Result = supervisor:start_child(?MODULE, Server),
    case Result of
        {error, already_present} ->
            supervisor:terminate_child(?MODULE, ServerID),
            supervisor:delete_child(?MODULE, ServerID),
            supervisor:start_child(?MODULE, Server);

        {error,{already_started, _Pid}} ->
            supervisor:terminate_child(?MODULE, ServerID),
            supervisor:delete_child(?MODULE, ServerID),
            supervisor:start_child(?MODULE, Server);
        {ok, _} ->
            Result
    end.