% @Author: Oleg Zilberman
% @Date:   2022-10-10 15:14:47
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2022-11-01 14:46:32

-module(no_such_endpoint).
-export([init/2]).
-include("include/error_responses.hrl").
init(Req0, State) ->
    Req = handle(Req0, State),
    {ok, Req, State}.


handle(Req, State) -> 
  case cowboy_req:method(Req) of
    <<"POST">> -> 
      Body = cowboy_req:has_body(Req),
      Request = reply(post, Body, Req),
        {ok, Request, State};
    <<"GET">> -> 
      #{id := Id} = cowboy_req:match_qs([{id, [], undefined}], Req),
      Request = reply(get, Id, Req),
        {ok, Request, State};
    <<"PUT">> -> 
      Body = cowboy_req:has_body(Req),
      Request = reply(put, Body, Req),
        {ok, Request, State}
  end.

  reply(post, _Body, Req) -> 
    cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, parse_request(Req), Req);

  reply(get, _Id, Req) -> 
    cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, parse_request(Req), Req);

  reply(put, _Body, Req) -> 
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
