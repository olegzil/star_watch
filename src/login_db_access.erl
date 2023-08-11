% @Author: oleg
% @Date:   2023-06-27 18:53:00
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-06-27 18:53:00
-include_lib("stdlib/include/ms_transform.hrl").
-include("include/macro_definitions.hrl").
-module(login_db_access).
-export([get_user_profile/2, 
        create_user_profile/4, 
        clear_login_table/0, 
        get_user_profile_from_token/1, 
        delete_user/2, 
        update_user_profile/3,
        update_user_profile/1,
        mark_user_as_logged_in/1,
        logout_user/1]).

-include("include/users_login_table.hrl").

logout_user(ClientID) ->
    Match = ets:fun2ms(
        fun(Record) 
            when Record#users_login_table.client_id =:= ClientID -> 
                Record
        end),

    SelectRecords = fun() -> mnesia:select(users_login_table, Match) end,
    case mnesia:transaction(SelectRecords) of
        {atomic, []} ->
            utils:format_error(?LOGIN_STATE_USER_NOT_FOUND, <<"user not found">>);
        {atomic, [ClientRecord]} ->
            WriteFun = fun() -> mnesia:write(ClientRecord#users_login_table{log_in_state = ?LOGIN_STATE_LOGGEDOUT}) end,
            mnesia:transaction(WriteFun),
            utils:format_success(?LOGIN_STATE_NOT_LOGGED_IN, <<"user logged out">>)
    end.

delete_user(token, Token) ->
    case get_user_profile_from_token(Token) of
        {ok, Record} ->
            Fun = fun() -> mnesia:delete({users_login_table, Record#users_login_table.user_id}) end,
            mnesia:transaction(Fun);
        _ ->
            io:format("No such token: ~p~n", [Token])
    end.

get_user_profile_from_token(Token) ->
    Fun = fun() -> mnesia:match_object(#users_login_table{login_token=Token, _='_'}) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, []} -> 
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
        {atomic, [Record]} ->
        	Result = #{
        		user_id => Record#users_login_table.user_id,
        		client_id => Record#users_login_table.client_id,
        		log_in_time => Record#users_login_table.log_in_time,
                log_in_state => Record#users_login_table.log_in_state,
                user_validated => Record#users_login_table.user_validated,
        		error_text => <<"">>
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
        {atomic, [Record]} ->
            Result = #{
                user_id=>Record#users_login_table.user_id,
                client_id=>Record#users_login_table.client_id,
                user_password=>Record#users_login_table.user_password,
                pending_password=>Record#users_login_table.pending_password,
                login_token=>Record#users_login_table.login_token,
                log_in_time=>Record#users_login_table.log_in_time,
                log_in_state=>Record#users_login_table.log_in_state,
                user_validated =>Record#users_login_table.user_validated,
                permitions => Record#users_login_table.permitions
            },
            {ok, Result}
    end.
update_user_profile(DataMap) -> 
    Record = #users_login_table{        
                user_id = maps:get(user_id,DataMap),
                client_id = maps:get(client_id,DataMap),
                user_password =maps:get(user_password,DataMap),
                login_token = maps:get(login_token,DataMap),
                log_in_time = maps:get(log_in_time,DataMap),
                log_in_state = maps:get(log_in_state,DataMap),
                user_validated = maps:get(user_validated, DataMap),
                permitions = maps:get(permitions,DataMap)
        },
    WriteFun = fun() -> mnesia:write(Record) end,
    mnesia:transaction(WriteFun),
    {ok, Record}.

mark_user_as_logged_in(Email) ->
    case login_db_access:get_user_profile(private, Email) of 
        {aborted, Reason} ->
            io:format("error reading users_login_table: ~p~n", [Reason]),
            utils:format_error(?SERVER_ERROR_DATABASE_FAULT, <<"unable to read user login data">>);
        {error, no_records} ->
            {error, no_records};
        {ok, Result} ->
            UpdatedRecord = maps:merge(Result, #{
                log_in_time => erlang:system_time(millisecond),
                log_in_state => ?LOGIN_STATE_LOGGED_IN
                }),
            update_user_profile(UpdatedRecord)
    end.

update_user_profile(LoginID, Member, NewValue) ->
    ReaderFun = fun() -> mnesia:read(users_login_table, LoginID) end, 
    case mnesia:transaction(ReaderFun) of
        {aborted,{Reason,_}} -> 
            {aborted, Reason};
        {atomic, []} ->
            {error, no_records};
        {_, [Record]} ->
            case Member of
                user_password ->
                    UpdateRecord = Record#users_login_table{user_password = NewValue},
                    WriteFun = fun()-> mnesia:write(UpdateRecord) end,
                    mnesia:transaction(WriteFun);
                login_token ->
                    UpdateRecord = Record#users_login_table{login_token = NewValue},
                    WriteFun = fun() -> mnesia:write(UpdateRecord) end,
                    mnesia:transaction(WriteFun);
                log_in_time ->
                    UpdateRecord = Record#users_login_table{log_in_time = NewValue},
                    WriteFun = fun() -> mnesia:write(UpdateRecord) end,
                    mnesia:transaction(WriteFun);
                log_in_state ->
                    UpdateRecord = Record#users_login_table{log_in_state = NewValue},
                    WriteFun = fun() -> mnesia:write(UpdateRecord) end,
                    mnesia:transaction(WriteFun);
                permitions ->
                    UpdateRecord = Record#users_login_table{permitions = NewValue},
                    WriteFun = fun() -> mnesia:write(UpdateRecord) end,
                    mnesia:transaction(WriteFun);
                pending_password ->
                    UpdateRecord = Record#users_login_table{pending_password = NewValue},
                    WriteFun = fun() -> mnesia:write(UpdateRecord) end,
                    mnesia:transaction(WriteFun);
                user_validated ->
                    UpdateRecord = Record#users_login_table{user_validated = NewValue},
                    WriteFun = fun() -> mnesia:write(UpdateRecord) end,
                    mnesia:transaction(WriteFun)                    
            end,
            {atomic, [NewRecord]} = mnesia:transaction(ReaderFun),
            Result = #{
                user_id=>NewRecord#users_login_table.user_id,
                client_id=>NewRecord#users_login_table.client_id,
                user_password=>NewRecord#users_login_table.user_password,
                login_token=>NewRecord#users_login_table.login_token,
                log_in_time=>NewRecord#users_login_table.log_in_time,
                log_in_state=>NewRecord#users_login_table.log_in_state,
                permitions => NewRecord#users_login_table.permitions
            },
            {ok, Result}
    end.

create_user_profile(ClientID, UserID, Password, Token) ->
    UserProfile = #users_login_table{
        user_id=UserID, 
        client_id=ClientID, 
        user_password=Password,
        login_token=Token,
        log_in_time=erlang:system_time(millisecond),
        log_in_state=?LOGIN_STATE_EMAIL_SENT,
        user_validated = true,
        permitions = 0
    },
    WriteFun = fun() -> mnesia:write(UserProfile) end,
    mnesia:transaction(WriteFun),
    UserProfile.
clear_login_table() ->
    mnesia:clear_table(users_login_table).
