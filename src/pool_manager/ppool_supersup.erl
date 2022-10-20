-module(ppool_supersup).
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ppool}, ?MODULE, []).

%% technically, a supervisor can not be killed in an easy way.
%% Let's do it brutally!
stop() ->
    case whereis(ppool) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.
% This methods starts(creates) a pool supevisor.
% The parameters are:
% Name -- unique name identifying this pool.
% Limit -- total number of workers that can be active before new requests are placed in a queue
% MFA -- module, function, arguments. Module name of the worker process, the entry point to the worker
%        and a set of possibly zero parameters for the worker.
start_pool(Name, Limit, MFA) ->
    ChildSpec = {Name,
                 {ppool_sup, start_link, [Name, Limit, MFA]},
                  permanent, infinity, supervisor, [ppool_sup]},
    supervisor:start_child(ppool, ChildSpec).

% Terminates a previously created ppool supervisor and all its workers.
stop_pool(Name) ->
    supervisor:terminate_child(ppool, Name),
    supervisor:delete_child(ppool, Name).

init([]) ->
    {ok, {{one_for_one, 6, 3600}, []}}.