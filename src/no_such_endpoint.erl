% @Author: Oleg Zilberman
% @Date:   2022-10-10 15:14:47
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-03-20 17:29:49

-module(no_such_endpoint).
-export([init/2]).
init(Req0, State) ->
    Req = handle(Req0, State),
    {ok, Req, State}.


handle(Req, State) -> 
  case cowboy_req:method(Req) of
    <<"GET">> -> 
      Request = reply(Req),
        {ok, Request, State};
		_ ->
		{error, Req, State}
  end.

  reply(Req) -> 
    cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, parse_request(Req), Req).

%%% 
%%% Error condition. The client did not specify a valid end-point.
%%% 
parse_request(Request) ->
	Headers = maps:get(headers, Request),
	Endpoint = maps:find(<<"referer">>, Headers),
  ErrorResponse = 
  case Endpoint of
    {ok, Value} ->
      {error, Error} = utils:format_error(-1, <<"No such endpoint: ", Value/binary>>),
      Error;
    error ->
      {error, Error} = utils:format_error(-1, maps:get(path, Request)),
      utils:log_message([{"bad path", maps:get(path, Request)}]),
      Error
  end,
  jiffy:encode(ErrorResponse).

