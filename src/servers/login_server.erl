% @Author: Oleg Zilberman
% @Date:   2023-03-11 18:33:02
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-28 19:26:51
-module(login_server).
-behaviour(gen_server).
-include("include/macro_definitions.hrl").
-include("include/users_login_table.hrl").
-inculude("include/macro_definitions.hrl").
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

handle_call({logout_user, ClientID}, _From, State) ->
    case login_db_access:logout_user(ClientID) of
        {error, Message} ->
            {reply, {error, Message}, State};
        {ok, Message} ->
            {reply, {ok, Message}, State}
    end;

handle_call({login_existing, _ClientID, EncryptedData}, _From, State) ->
    case query_user_login(EncryptedData) of 
        {error, no_records} ->
            {error, Message} = utils:format_error(?LOGIN_STATE_USER_NOT_FOUND, <<"user profile not found">>),
            {reply, {error, Message}, State};
        {error, Message} ->
            {reply, {error, Message}, State};
        {expired, UserMap} ->
            login_db_access:mark_user_as_logged_in(maps:get(user_id, UserMap)),
            Result = utils:format_success(?LOGIN_STATE_LOGIN_EXPIRED, <<"session expired">>),
            {reply, Result, State};
        {ok, UserMap} ->
            login_db_access:mark_user_as_logged_in(maps:get(user_id, UserMap)),
            Result = utils:format_success(?LOGIN_STATE_LOGGED_IN, <<"successful login">>),
            {reply, Result, State}

    end;

handle_call({user_profile, _ClientID, EncryptedData}, _From, State) ->
    case query_user_login_id(EncryptedData) of 
        {error, no_records} ->
            {error, Message} = utils:format_error(?LOGIN_STATE_USER_NOT_FOUND, <<"user profile not found">>),
            {reply, {error, Message}, State};
        {expired, Map} ->
            {ok, Message} = utils:format_login_server_return(?LOGIN_STATE_TOKEN_EXPIRED, Map, <<"expired token">>),
            {reply, {ok, Message}, State};
        {error, Message} ->
            {reply, {error, Message}, State};
        {ok, UserMap} ->
            {reply, {ok, UserMap}, State}
    end;


handle_call({login_new_userid, ClientID, EncryptedData}, _From, State) ->
    case utils:extract_id_and_password(EncryptedData) of
        {Email, Password} ->
            Profile = login_db_access:get_user_profile(private, Email),
            CreateNewUserResult = handle_new_user_id(userid, Profile, ClientID, Email, Password),
            {reply, CreateNewUserResult, State};
        error ->
            {error, Error} = utils:format_error(false, <<"invalid login string">>),
            {reply, {error, Error}, State}
    end;

handle_call({complete_login, Token}, _From, State) ->
    io:format("from handle_call:complete_login~n"),
    Profile=login_db_access:get_user_profile_from_token(Token),
    case Profile of
        {error, _} ->
            {error, Error} = utils:format_error(?SERVER_ERROR_NO_SUCH_USER_ID, <<"no such token: ", Token/binary>>),
            {error, Error};
        {ok, Record} ->
            TimeDelta = erlang:system_time(millisecond) - Record#users_login_table.log_in_time,
            if
                TimeDelta > ?LOGIN_TOKEN_EXPIRATION_TIME ->
                    {error, Error} = utils:format_error(?LOGIN_TOKEN_EXPIRATION_TIME, <<"expired token">>),
                    login_db_access:delete_user(token, Token),
                    {reply, {error, Error}, State};
                true ->
                    Email = Record#users_login_table.user_id,
                    complete_registration(Email),
                    {ok, Success} = utils:format_success(?SERVER_ERROR_OK, <<"Email registered: ", Email/binary>>),
                    {reply, {ok, Success}, State}
            end
    end;

handle_call({clear_login_table}, _From, State) ->
    login_db_access:clear_login_table(),
    {reply, {ok, #{success => "login table cleared"}}, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Private Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
complete_registration(Email)->
    UpdateFun = fun()->
        [Record] = mnesia:read(users_login_table, Email, write),
        mnesia:write(Record#users_login_table{
                    login_token = undefined,
                    log_in_time = erlang:system_time(millisecond),
                    log_in_state = ?LOGIN_STATE_LOGGED_IN,
                    user_validated = true
            })
    end,
    mnesia:transaction(UpdateFun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This function validates the current login states   %%%
%%% The return value is a tuple where the first   %%%
%%% member is either error or ok. ok Is returned only  %%% 
%%% if the user is currently loged in. Any other state %%%
%%% is considered an error states                      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
validate_email_state(Map) ->
    VerificationState = maps:get(log_in_state, Map),
    case VerificationState of
        ?LOGIN_STATE_USER_NOT_FOUND ->
            Email = maps:get(user_id, Map),
            utils:format_error(?LOGIN_STATE_USER_NOT_FOUND, <<"user profile does not exist " , Email/binary>>);
        ?LOGIN_STATE_EMAIL_SENT ->
            Email = maps:get(user_id, Map),
            utils:format_error(?LOGIN_STATE_EMAIL_SENT, <<"email sent. waiting for response " , Email/binary>>);
        ?LOGIN_STATE_TOKEN_EXPIRED -> 
            Email = maps:get(user_id, Map),
            utils:format_error(?LOGIN_STATE_TOKEN_EXPIRED, <<"Attempt to validate email with expired token " , Email/binary>>);
        ?LOGIN_STATE_CONFIRMED ->
            Email = maps:get(user_id, Map),
            utils:format_success(?LOGIN_STATE_CONFIRMED, <<"user login confirmed ", Email/binary>>);  
        ?LOGIN_STATE_LOGGED_IN -> 
            Email = maps:get(user_id, Map),
            utils:format_success(?LOGIN_STATE_LOGGED_IN, <<"user is logged in ", Email/binary>>);
        ?LOGIN_STATE_LOGGEDOUT ->
            Email = maps:get(user_id, Map),
            utils:format_error(?LOGIN_STATE_LOGGEDOUT, <<"user is not logged in ", Email/binary>>);      
        ?LOGIN_STATE_RESET_PASSWORD ->
            Email = maps:get(user_id, Map),
            utils:format_error(?LOGIN_STATE_RESET_PASSWORD, <<"Password reset requested ", Email/binary>>);      
        ?LOGIN_STATE_RESET_USER ->
            Email = maps:get(user_id, Map),
            utils:format_error(?LOGIN_STATE_RESET_PASSWORD, <<"Email reset requested ", Email/binary>>)                
    end.

handle_new_user_id(userid, UserProfile, ClientID, Email, Password) ->
    case UserProfile of
        {aborted, Reason} ->
            {error, Reason};
        {error, no_records} ->
            case extract_user_name(Email) of 
                {ok, {User, _}} ->
                    utils:log_message([{"Email", Email}]),
                    EmeailResult = mail_utility:send_email(Email, list_to_binary(User)),
                    check_email_sent_result(EmeailResult, ClientID, Email, Password);
                {error, Result} ->
                    utils:format_error(?SERVER_ERROR_INVALID_EMAIL, <<"Invalid email address: ", Result/binary>>)
            end;
        {_, Map} ->
            validate_email_state(Map)
    end.

extract_user_name(Email) ->
    case string:tokens(binary_to_list(Email), "@") of
        [User, EmailHost] ->
            {ok, {User, EmailHost}};
        [Result] ->
            {error, list_to_binary(Result)}
    end.

check_email_sent_result(EmailResponse, ClientID, Email, Password) ->            
    case EmailResponse of
        {ok, Token} ->
            login_db_access:create_user_profile(ClientID, Email, Password, Token),
            utils:format_success(?LOGIN_STATE_EMAIL_SENT, <<"email sent">>);
        {error, ErrorCode} ->
            utils:format_error(ErrorCode, <<"email notification error">>)
    end.

query_user_login_id(EncryptedData) ->
    Result = utils:extract_id(EncryptedData),
    utils:log_message([{"Result", Result}]),
    case Result of
        {ok, Email} ->
            process_query_request(Email);
        {eror, Error} ->
            utils:format_error(?SERVER_ERROR_NO_SUCH_USER_ID, <<"invalid credentials", Error/binary>>)
    end.
query_user_login(EncryptedData) -> 
    {Email, Password} = utils:extract_id_and_password(EncryptedData),
    UserProfile = login_db_access:get_user_profile(private, Email),
    case process_user_profile_query_result(UserProfile) of
        {error, Message} ->
            {error, Message};
        {_, UserMap} ->
            UserPassword = maps:get(user_password, UserMap),
            if
                UserPassword =:= Password ->
                    {ok, private_profile_to_public(UserMap)};
                true ->
                    utils:format_error(?LOGIN_ERROR_PASSWORD, <<"invalid password">>)
            end
    end.
private_profile_to_public(UserMap) ->
    #{
        user_id  => maps:get(user_id, UserMap),
        log_in_state => maps:get(log_in_state, UserMap),
        log_in_time => maps:get(log_in_time, UserMap),
        client_id => maps:get(client_id, UserMap)
    }.

process_query_request(Email) ->
    UserProfile = login_db_access:get_user_profile(public, Email),
    process_user_profile_query_result(UserProfile).

process_user_profile_query_result(Result) ->
    utils:log_message([{"Result", Result}]),
    case Result of
        {aborted, Reason} ->
            io:format("error reading users_login_table: ~p~n", [Reason]),
            utils:format_error(?SERVER_ERROR_DATABASE_FAULT, <<"unable to read user login data">>);
        {error, no_records} ->
            {error, no_records};
        {ok, Map} ->
            LoginTime = maps:get(log_in_time, Map),
            Email = maps:get(user_id, Map),
            TimeDelta = erlang:system_time(millisecond) - LoginTime,
            if
                TimeDelta > ?LOGIN_TOKEN_EXPIRATION_TIME ->
                    login_db_access:update_user_profile(Email, login_token, ?LOGIN_STATE_TOKEN_EXPIRED),
                    {expired, Map};
                true ->
                    {ok, Map}
            end
    end.
