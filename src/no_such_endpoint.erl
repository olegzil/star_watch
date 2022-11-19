% @Author: Oleg Zilberman
% @Date:   2022-10-10 15:14:47
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-11-17 10:37:24

-module(no_such_endpoint).
-export([init/2]).
-include("include/error_responses.hrl").
init(Req0, State) ->
    Req = handle(Req0, State),
    {ok, Req, State}.


handle(Req, State) -> 
  case cowboy_req:method(Req) of
    <<"GET">> -> 
      #{id := Id} = cowboy_req:match_qs([{id, [], undefined}], Req),
      Request = reply(get, Id, Req),
        {ok, Request, State}
  end.

  reply(get, _Id, Req) -> 
    cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, parse_request(Req), Req).

%%% 
%%% Error condition. The client did not specify a valid end-point.
%%% 
parse_request(Request) ->
	Title = <<"Unknown endpoint: ">>,
	Message = cowboy_req:path(Request),
	ErrorResponse = #{
		date_time => utils:current_time_string(),
		error_code => 404,
		error_text => <<Title/binary, Message/binary>>
	},
	jiffy:encode(ErrorResponse).
