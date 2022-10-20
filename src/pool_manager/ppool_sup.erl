% @Author: oleg
% @Date:   2022-02-10 16:08:38
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-10-19 17:33:32
-module(ppool_sup).
-export([start_link/3, init/1]).
-behaviour(supervisor).


% This is the supervisor for a single pool.
% Name -- a unique name of this pool supervisor
% Limit -- the maximum number of workers this supevisor allowes before they head for the queue
% MFA	-- the Module Function Args of the worker.
start_link(Name, Limit, MFA) ->
	supervisor:start_link(?MODULE, {Name, Limit, MFA}).

% This supervisor is considered to be a worker for the ppool_supersup module and is marked a such
% Note that the Name is passed to the server, along with self(), the supervisor's own pid. 
% This will let the server call for the spawning of the worker supervisor; the MFA variable 
% will be used in that call to let the simple_one_for_one supervisor know what kind of 
% workers to run.

init({Name, Limit, MFA}) ->
	MaxRestart = 1,
	MaxTime = 3600,
	{ok, {{one_for_all, MaxRestart, MaxTime},
		  [{serv, 
		     {ppool_serv, start_link, [Name, Limit, self(), MFA]},
		      permanent, 
		      5000,
		      worker,
		      [ppool_serv]}
		   ]
		  }
	}.