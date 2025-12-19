-module(snetd_auth_cookie).
-behaviour(cowboy_handler).
-export([routes/0]).
-export([init/2]).

%% Public

routes() ->
	[{"/snet/auth/cookie", snetd_auth_cookie, []}].

%% cowboy_handler

init(#{ method := ~"POST" } = Req, _) ->
	Headers = #{ ~"content-type" => ~"text/plain" },
	maybe
		BodyOpts = #{ length => 2048, period => 5000 },
		{ok, Body, Req2} ?= case cowboy_req:read_body(Req, BodyOpts) of
			{ok, _, _} = ReadBody ->
				ReadBody;
			{more, _, Req2In} ->
				{return, cowboy_req:reply(400, Req2In)}
		end,
		{ok, UserId} ?= case snetd_auth:verify_token(Body) of
			{ok, _} = Verified ->
				Verified;
			{error, _} ->
				{return, cowboy_req:reply(400, Req2)}
		end,
		Cookie = snetd_auth:issue_token(UserId),
		Req3 = cowboy_req:reply(200, Headers, Cookie, Req2),
		{ok, Req3, []}
	else
		{return, ReturnReq} -> {ok, ReturnReq, []};
		{error, _} -> {ok, cowboy_req:reply(500, Req), []}
	end;
init(Req, State) ->
	{ok, cowboy_req:reply(400, Req), State}.
