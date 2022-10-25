% @Author: oleg
% @Date:   2022-02-10 16:22:17
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-10-21 14:16:48
-module(ppool_serv).
-behaviour(gen_server).
-export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% NOTICE NOTICE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   All comments are coppied from learnyousomeerlang.com                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The friendly supervisor is started dynamically!
%% Child specification of the worker supervisor 
-define(SPEC(MFA),
        {worker_sup,
         {ppool_worker_sup, start_link, [MFA]},
          temporary,
          10000,
          supervisor,
          [ppool_worker_sup]}).

%% This record tracks:
%% limit -- max number of processes that can run under this supervisor
%% sup -- the Pid of the supervisor
%% refs -- in-memeory references to running jobs (not waiting to run)
%% queue -- a queue containing all the running jobs.

-record(state, {limit=0,
                sup,
                refs,
                queue=queue:new(),
                maxlimit = 0}).

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

run(Name, Args) ->
    gen_server:call(Name, {run, Args}).

sync_queue(Name, Args) ->
    gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
    gen_server:cast(Name, {async, Args}).

stop(Name) ->
    gen_server:call(Name, stop).

%% Gen server
%% using self() ! {start_worker_supervisor, Sup, MFA} prevents a dead lock. 
init({Limit, MFA, Sup}) ->
    %% We need to find the Pid of the worker supervisor from here,
    %% but alas, this would be calling the supervisor while it waits for us!
    self() ! {start_worker_supervisor, Sup, MFA},
    {ok, #state{limit=Limit, refs=gb_sets:empty(), maxlimit=Limit}}.

%% Whenever there are places left in the pool (the original limit N being decided by the 
%% programmer adding the pool in the first place), we accept to start the worker. We then 
%% set up a monitor to know when it's done, store all of this in our state, decrement the 
%% counter and off we go.
handle_call({run, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    io:format("Limit = ~p~n", [N]),
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok,Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};


handle_call({run, _Args}, _From, S=#state{limit=N, maxlimit = MaxLimit}) when N =< 0 ->
    io:format("~n**************************************~n", []),
    io:format("Worker limit reached: Limit = ~p~n", [MaxLimit]),
    io:format("~n**************************************~n", []),
    {reply, noalloc, S};

%% If there is space for more workers, then the first clause is going to do exactly the same 
%% as we did for run/2. The difference comes in the case where no workers can run. Rather than 
%% replying with noalloc as we did last time, this one doesn't reply to the caller, keeps the 
%% From information and enqueues it for a later time when there is space for the worker to be 
%% run. 
handle_call({sync, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok,Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_call({sync, Args},  From, S = #state{queue=Q}) ->
    {noreply, S#state{queue=queue:in({From, Args}, Q)}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.


handle_cast({async, Args}, S=#state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {noreply, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_cast({async, Args}, S=#state{limit=N, queue=Q}) when N =< 0 ->
    {noreply, S#state{queue=queue:in(Args,Q)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs=Refs}) ->
    case gb_sets:is_element(Ref, Refs) of
        true ->
            handle_down_worker(Ref, S);
        false -> %% Not our responsibility
            {noreply, S}
    end;

%% Start the worker and get its Pid. Then link to it to be notified of its demise.
handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    link(Pid),
    io:format("ppool_serv:handle_info~nMFA: ~p~nSup: ~p~n", [MFA, Sup]),
    {noreply, S#state{sup=Pid}};
handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% Because our worker is dead, we can look in the queue for the next one to run. We do this 
%% by popping one element out of the queue, and looking what the result is. If there is at 
%% least one element in the queue, it will be of the form {{value, Item}, NewQueue}. If the 
%% queue is empty, it returns {empty, SameQueue}. Furthermore, we know that when we have 
%% the value {From, Args}, it means this came from sync_queue/2 and that it came from 
%% async_queue/2 otherwise.
handle_down_worker(Ref, S = #state{limit=L, sup=Sup, refs=Refs}) ->
    case queue:out(S#state.queue) of
        {{value, {From, Args}}, Q} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
            gen_server:reply(From, {ok, Pid}),
            {noreply, S#state{refs=NewRefs, queue=Q}};
        {{value, Args}, Q} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
            {noreply, S#state{refs=NewRefs, queue=Q}};
        {empty, _} ->
            {noreply, S#state{limit=L+1, refs=gb_sets:delete(Ref,Refs)}}
    end.