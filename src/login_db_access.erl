% @Author: oleg
% @Date:   2023-06-27 18:53:00
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-06-27 18:53:00
-include_lib("stdlib/include/ms_transform.hrl").
-module(login_db_access).
-export([get_user_profile/1, create_user_profile/3, clear_login_table/0]).
-include("include/users_login_table.hrl").

get_user_profile(LoginID) ->
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
    end.
create_user_profile(ClientID, UserID, Password) ->
    UserProfile = #users_login_table{
        user_id=UserID, 
        client_id=ClientID, 
        user_password=Password,
        loged_in=false,
        log_in_time=undefined,
        credentials_verified=false
    },
    WriteFun = fun() -> mnesia:write(UserProfile) end,
    mnesia:transaction(WriteFun),
    UserProfile.
clear_login_table() ->
    mnesia:clear_table(users_login_table).
