% @Author: oleg
% @Date:   2022-02-10 16:15:45
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-10-20 15:43:22
-module(ppool_worker_sup).
-export([start_link/1, init/1]).
-behaviour(supervisor).

% This is the worker supevisor, i.e., a supervisor that creates workers 
% as opposed to other supervisors

start_link(MFA = {_,_,_}) ->
    supervisor:start_link(?MODULE, MFA).

init({M,F,A}) ->
    {ok, {{simple_one_for_one, 5, 3600},
          [{ppool_worker,
            {M,F,A},
            temporary, 5000, worker, [M]}]}}.
    