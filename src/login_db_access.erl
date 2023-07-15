% @Author: oleg
% @Date:   2023-06-27 18:53:00
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-06-27 18:53:00
-include_lib("stdlib/include/ms_transform.hrl").
-include("include/macro_definitions.hrl").
-module(login_db_access).
-export([get_user_profile/2, create_user_profile/4, clear_login_table/0, get_user_profile_from_token/1]).
-include("include/users_login_table.hrl").

get_user_profile_from_token(Token) ->
    Fun = fun() -> mnesia:match_object(#users_login_table{login_token=Token, _='_'}) end,
    Result = mnesia:transaction(Fun),
    case Result of
        [] -> 
            {error, <<"not found">>};
        {atomic, [Record]} ->
            {ok, Record}
    end.

get_user_profile(public, LoginID) ->
    ReaderFun = fun() -> mnesia:read(users_login_table, LoginID) end, 
    case mnesia:transaction(ReaderFun) of
        {aborted,{Reason,_}} -> 
            {aborted, Reason};
        {atomic, []} ->
            {error, no_records};
        {_, [Record]} ->
        	Result = #{
        		user_id => Record#users_login_table.user_id,
        		client_id => Record#users_login_table.client_id,
        		loged_in => Record#users_login_table.loged_in,
        		log_in_time => Record#users_login_table.log_in_time,
        		error_text =>""
        	},
        	{ok, Result}
    end;

get_user_profile(private, LoginID) ->
    ReaderFun = fun() -> mnesia:read(users_login_table, LoginID) end, 
    case mnesia:transaction(ReaderFun) of
        {aborted,{Reason,_}} -> 
            {aborted, Reason};
        {atomic, []} ->
            {error, no_records};
        {_, [Record]} ->
            Result = #{
                user_id=>Record#users_login_table.user_id,
                client_id=>Record#users_login_table.client_id,
                user_password=>Record#users_login_table.user_password,
                login_token=>Record#users_login_table.login_token,
                loged_in=>Record#users_login_table.loged_in,
                log_in_time=>Record#users_login_table.log_in_time,
                credentials_verified=>Record#users_login_table.credentials_verified,
                permitions => Record#users_login_table.permitions
            },
            {ok, Result}
    end.

create_user_profile(ClientID, UserID, Password, Token) ->
    UserProfile = #users_login_table{
        user_id=UserID, 
        client_id=ClientID, 
        user_password=Password,
        login_token=Token,
        loged_in=false,
        log_in_time=erlang:system_time(millisecond),
        credentials_verified=?LOGIN_STATE_EMAIL_SENT,
        permitions = 0
    },
    WriteFun = fun() -> mnesia:write(UserProfile) end,
    mnesia:transaction(WriteFun),
    UserProfile.
clear_login_table() ->
    mnesia:clear_table(users_login_table).
