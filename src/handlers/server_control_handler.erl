% @Author: Oleg Zilberman
% @Date:   2023-02-06 18:53:32
% @Last Modified by:   Oleg Zilberman
% @Last Modified time: 2023-02-07 09:26:06
-module(server_control_handler).
-behavior(cowboy_handler).
-include("include/macro_definitions.hrl").
-export([init/2]).

init(Req0, State) ->
    handle(Req0, State).


handle(Req, State) -> 
  case cowboy_req:method(Req) of
    <<"GET">> -> 
      Body = cowboy_req:has_body(Req),
      Request = process_request(Body, Req),
      {ok, Request, State};
    _ ->
      {error, Req, State}
  end.

process_request(_Body, Request) ->
    try #{
        action := Action
    } = cowboy_req:match_qs([{page, [], <<"1">>}, action, api_key], Request) of
         _ ->
         	{Verb, Subject} = get_verb_subject(Action),
            Response = execute_action(Verb, Subject),
            cowboy_req:reply(200,  #{<<"content-type">> => <<"application/json; charset=utf-8">>}, Response, Request)
     catch
         _:Error ->
            {_, {_, Term}, _} = Error,
            io:format("***** Error *****: ~p~n", [Term]),
            jiffy:encode(Term)     
    end.
    
execute_action(Verb, Subject) ->
	io:format("Verb: ~p~nSubject: ~p~n", [Verb, Subject]),
	case Verb of
		backup ->
			RetVal = mnesia:backup(Subject),
			jiffy:encode(#{code => RetVal, message => <<"backup succeeded">>,file => list_to_binary(Subject)});
		restore ->
			try mnesia:restore(Subject, [{keep_tables, [apodimagetable, apodtelemetry]}]) of
				{atomic,_} ->
					jiffy:encode(#{message => <<"data restored">>,file => list_to_binary(Subject)});
				{aborted, Reason} ->
					[File, _Name, _Repair, _Mode, _Link] = element(2, Reason),
					jiffy:encode(#{message => <<"aborted">>, errors => element(2, File)})
			catch
				{Error, Reason} ->
					io:format("Message 3~n"),
					jiffy:encode(#{message => <<Error>>, errors => <<Reason>>})
			end;
		_ ->
			jiffy:encode(#{message => <<"unknow action">>, action => Verb})
	end.

get_verb_subject(Action) ->
	case string:split(Action, ":") of
		[Verb, <<>>] ->
			Name = generate_file_name(),
			{binary_to_atom(Verb), Name};
		[Verb] ->
			Name = generate_file_name(),
			{binary_to_atom(Verb), Name};
		[Verb, Subject] ->
			{binary_to_atom(Verb), binary_to_list(Subject)}
	end.

generate_file_name() ->
	{{Year, Month, Day}, {H, M, S}} = calendar:local_time(),
	Date =io_lib:format("~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w",   [Year, Month, Day, H, M, S]),
	Part1 = string:concat("star_watch_db_", Date),
	Temp = string:concat(Part1, ".backup"),
	FullName = list_to_binary(Temp),
	FileName = binary_to_list(FullName),
	FileName.
