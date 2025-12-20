-module(snetd_utils).
-export([
	handle_early_return/2,
	reply_with_text/3,
	reply_with_json/3,
	read_body/2,
	read_body_json/2
]).
-include_lib("kernel/include/logger.hrl").

-spec reply_with_text(cowboy:http_status(), iodata(), cowboy_req:req()) -> cowboy_req:req().
reply_with_text(Status, Text, Req) ->
	cowboy_req:reply(
		Status,
		#{ ~"content-type" => "text/plain" },
		Text,
		Req
	).

-spec reply_with_json(cowboy:http_status(), term(), cowboy_req:req()) -> cowboy_req:req().
reply_with_json(Status, Json, Req) ->
	cowboy_req:reply(
		Status,
		#{ ~"content-type" => "application/json" },
		json:encode(Json),
		Req
	).

-spec read_body(Req, cowboy_req:read_body_opts()) ->
	{ok, binary(), Req} | {error, entity_too_large, Req}
	  when Req :: cowboy_req:req().
read_body(Req, Opts) ->
	Length = maps:get(length, Opts, 1024 * 1024),
	case cowboy_req:read_body(Req, Opts) of
		{ok, Body, _} = ReadBody when byte_size(Body) =< Length ->
			ReadBody;
		{ok, _, Req2} ->
			{error, entity_too_large, Req2};
		{more, _, Req2} ->
			{error, entity_too_large, Req2}
	end.

-spec read_body_json(Req, cowboy_req:read_body_opts()) ->
	{ok, json:decode_value(), Req} | {error, entity_too_large | bad_request, Req}
	  when Req :: cowboy_req:req().
read_body_json(Req, Opts) ->
	case read_body(Req, Opts) of
		{ok, Body, Req2} ->
			try json:decode(Body) of
				Json -> {ok, Json, Req2}
			catch
				error:_ -> {error, bad_request, Req2}
			end;
		{error, _, _} = Err ->
			Err
	end.

-spec handle_early_return(term(), cowboy_req:req()) -> {ok, cowboy_req:req(), []}.
handle_early_return({return, Req}, _) ->
	{ok, Req, []};
handle_early_return({error, {unauthorized, Reason}}, Req) ->
	?LOG_DEBUG(#{ what => unauthorized, reason => Reason }),
	{ok, reply_with_text(401, ~"unauthorized", Req), []};
handle_early_return({error, entity_too_large, Req}, _) ->
	{ok, reply_with_text(413, ~"entity_too_large", Req), []};
handle_early_return({error, bad_request, Req}, _) ->
	{ok, reply_with_text(400, ~"bad_request", Req), []};
handle_early_return(Value, Req) ->
	?LOG_ERROR(#{ what => unknown_return, error => Value }),
	{ok, reply_with_text(500, ~"internal_error", Req), []}.
