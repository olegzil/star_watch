% @Author: Oleg Zilberman
% @Date:   2023-01-14 10:24:43
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-01-14 11:48:29
-module(dispatch_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).
-export([start_link/0]).
-export([init/1]).
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [], []).

init([Child_Spec]) ->
    MaxRestart = 1,
    MaxTime = 5,
    io:format("Child_Spec: ~p~n", [Child_Spec]),
    {ok, {{one_for_one, MaxRestart, MaxTime}, Child_Spec}}.

stop() ->
    case whereis(?MODULE) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.
