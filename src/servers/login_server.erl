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

handle_call({login_existing, _ClientID, EncryptedData}, _From, State) ->
    case utils:decrypt_data(EncryptedData) of
        {ok, ClearText} ->
            {ok, Success} = utils:format_success(true, <<"user loged in successfully: ", ClearText/binary>>),
            {reply, {ok, Success}, State};
        {error, Error} ->
            {error, Error} = utils:format_error(false, Error),
            {reply, {ok, Error}, State}
    end;

handle_call({user_login_profile, EncryptedLoginID}, _From, State) ->
    utils:log_message([{"user_login_profile", user_login_profile}]),
    case utils:decrypt_data(EncryptedLoginID) of
        {ok, ClearText} ->
            query_user_login_id(ClearText);
        {error, Error} ->
            {error, Error} = utils:format_error(false, Error),
            {reply, {ok, Error}, State}
    end;

handle_call({login_new_userid, ClientID, EncryptedData}, _From, State) ->
    case extract_id_and_password(start, EncryptedData) of
        {UserID, Password} ->
            Profile = login_db_access:get_user_profile(UserID),
            CreateNewUserResult = handle_new_user_id(userid, Profile, ClientID, UserID, Password),
            {reply, CreateNewUserResult, State};
        error ->
            {error, Error} = utils:format_error(false, <<"invalid login string">>),
            {reply, {error, Error}, State}
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
extract_id_and_password(start, Data) ->
    {ok, ClearText} = utils:decrypt_data(Data),
    Parts = string:split(ClearText, ?LOGIN_ID_TOKEN),
    extract_id_and_password(validate_part1, Parts);

extract_id_and_password(validate_part1, Data) when length(Data) =:= 2 ->
    extract_id_and_password(validate_part2, Data);

extract_id_and_password(validate_part1, _Data) -> false;
extract_id_and_password(validate_part2, [_,Data]) ->
    Parts = string:split(Data, ?LOGIN_PASSWORD_TOKEN),
    extract_id_and_password(validate_part3, Parts);

extract_id_and_password(validate_part3, Data) when length(Data) =:= 2 ->
    [ID, Password] = Data,
    {ID, Password};
extract_id_and_password(validate_part3, _Data) -> false.

handle_new_user_id(userid, UserProfile, ClientID, UserID, Password) ->
    case UserProfile of
        {aborted, Reason} ->
            {error, Reason};
        {error, no_records} ->
            Profile = login_db_access:create_user_profile(ClientID, UserID, Password),
            {ok, ProfileMap} = generate_return_value(Profile),
            [_User, Host] = string:tokens(binary_to_list(UserID), "@"),
            utils:log_message([{"Host", Host}]),
            EmailBody =  "'Erlang test passed.'",
            EmailHeader = "'Erlang-Test-Email'",
            From = "-aFrom:TheTinkerersShop@gmail.com",
            To = binary_to_list(UserID),
            Parts = ["echo", EmailBody, "|", "mail -s", EmailHeader, From, To],
            CommandString = string:join(Parts, " "),
            os:cmd(CommandString),
            {ok, ProfileMap};
        {_, ExistingUser} ->
            utils:log_message([{"ExistingUser", ExistingUser}]),
            if
                ExistingUser#users_login_table.credentials_verified =:= false orelse
                ExistingUser#users_login_table.credentials_verified =:= undefined ->
                    Profile = login_db_access:create_user_profile(ClientID, UserID, Password),
                    generate_return_value(Profile);
                true ->
                    utils:format_error(?SERVER_ERROR_USER_EXISTS, <<"user exists: ", UserID/binary>>)
                end
    end.

generate_return_value(Profile) ->
    {ok, #{
                user_id=>Profile#users_login_table.user_id,
                client_id=>Profile#users_login_table.client_id,
                loged_in=>Profile#users_login_table.loged_in,
                log_in_time=> erlang:system_time(millisecond),
                credentials_verified=>false
              }
        }.

query_user_login_id(LoginID) ->
    case login_db_access:get_user_profile(LoginID) of 
        {aborted, Reason} ->
            io:format("error reading users_login_table: ~p~n", [Reason]),
            {error, ErrorPayload} = utils:format_error(?SERVER_ERROR_DATABASE_FAULT, <<"unable to read user login data">>),
            {error, jiffy:encode(ErrorPayload)};
        {error, no_records} ->
            {error, ErrorPayload} = utils:format_error(?SERVER_ERROR_NO_LOGIN_RECORD, <<"user login does not exist">>),
            {error, jiffy:encode(ErrorPayload)};
        {ok, Result} ->
            {ok, jiffy:encode(Result)}
    end.

