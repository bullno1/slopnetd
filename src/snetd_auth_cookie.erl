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
		{ok, Body, Req2} ?= snetd_utils:read_body(Req, BodyOpts),
		{ok, User} ?= case snetd_auth:verify_token(Body) of
			{ok, _} = Verified ->
				Verified;
			{error, _} ->
				{return, cowboy_req:reply(400, Req2)}
		end,
		Cookie = snetd_auth:issue_token(User),
		Req3 = cowboy_req:reply(200, Headers, Cookie, Req2),
		{ok, Req3, []}
	else
		EarlyReturn -> snetd_utils:handle_early_return(EarlyReturn, Req)
	end;
init(Req, State) ->
	{ok, cowboy_req:reply(400, Req), State}.
