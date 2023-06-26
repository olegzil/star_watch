% @Author: Oleg Zilberman
% @Date:   2023-03-11 18:33:02
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-28 19:26:51
-module(login_server).
-behaviour(gen_server).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init([{}]) ->
    {ok, [{}]}.
   

%%% OTP Callbacks
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({login_existing, _ClientID, EncryptedData}, _From, State) ->
    case decrypt_data(EncryptedData) of
        {ok, ClearText} ->
            {ok, Success} = utils:format_success(true, <<"user loged in successfully: ", ClearText/binary>>),
            Result = jiffy:encode(Success),
            {reply, {ok, Result}, State};
        {error, Error} ->
            {error, Error} = utils:format_error(false, Error),
            {reply, {ok, Error}, State}
    end;
handle_call({user_login_profile}, _From, State) ->
    ok;
handle_call({login_new, _ClientID, EncryptedData}, _From, State) ->
    % 1. Decrypt data
    % 2. Query db for login stage
    % 3. If this is ID verification
    % 3a.If ID does not exist, return success along with a password token
    % 3b.If ID is duplicate, return failure along with an ID token
    % 4. Repeat 3 .. 3b at most four times
    case decrypt_data(EncryptedData) of
        {ok, ClearText} ->
            Result = db_access:query_login_stage(ClearText),
            {reply, {ok, Result}, State};
        {error, Error} ->
            {error, Error} = utils:format_error(false, Error),
            {reply, {ok, Error}, State}
    end;

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Private functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decrypt_data(EncryptedData) ->
    {ok, PemBin} = server_config_processor:read_private_key_file(),
    [RSAEntry] = public_key:pem_decode(PemBin),
    Key = public_key:pem_entry_decode(RSAEntry),
    case utils:decrypt(Key, EncryptedData) of
        {ok, ClearText} ->
            {ok, ClearText};
        {error, Error} ->
            {error, Error}
    end.