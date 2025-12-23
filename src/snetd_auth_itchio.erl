-module(snetd_auth_itchio).
-behaviour(cowboy_handler).
-export([routes/0]).
-export([init/2]).

%% Public

routes() ->
	[
	  {"/snet/auth/itchio/start", snetd_auth_itchio, start}
	, {"/snet/auth/itchio/callback", cowboy_static, {priv_file, slopnetd, "www/itchio/callback.html"}}
	, {"/snet/auth/itchio/callback2", snetd_auth_itchio, callback}
	, {"/snet/auth/itchio/end", cowboy_static, {priv_file, slopnetd, "www/itchio/end.html"}}
	, {"/snet/auth/itchio/[...]", cowboy_static, {priv_dir, slopnetd, "www/itchio/assets"}}
	].

%% cowboy_handler

init(Req, start) ->
	#{ app_port := AppPort } = cowboy_req:match_qs([{app_port, int}], Req),
	{ok, #{
		client_id := ClientId,
		redirect_uri := RedirectUri
	}} = application:get_env(slopnetd, itchio),
	Qs = uri_string:compose_query([
		{~"client_id", ClientId},
		{~"redirect_uri", RedirectUri},
		{~"response_type", ~"token"},
		{~"scope", ~"profile:me game:view:ownership"},
		{~"state", integer_to_binary(AppPort)}
	]),
	RedirectUrl = uri_string:recompose(#{
		scheme => ~"https",
		host => ~"itch.io",
		path => ~"/user/oauth",
		query => Qs
	}),
	{ok, cowboy_req:reply(302, #{~"location" => RedirectUrl}, Req), []};
init(Req, callback) ->
	#{
		access_token := AccessToken,
		state := State
	} = cowboy_req:match_qs([{access_token, nonempty}, {state, int}], Req),
	{ok, #{
		game_id := GameId
	}} = application:get_env(slopnetd, itchio),
	maybe
		ok ?= case itchio_api([~"/games/", GameId, ~"/ownership"], AccessToken) of
			{ok, #{ ~"owns" := true}}->
				ok;
			{ok, _} ->
				{error, game_not_owned};
			{error, _} = ErrOwnership ->
				ErrOwnership
		end,
		{ok, Username} ?= case itchio_api(~"/profile", AccessToken) of
			{ok, #{ ~"user" := #{ ~"username" := UsernameIn }}} when is_binary(UsernameIn) ->
				{ok, UsernameIn};
			{ok, _} ->
				{error, invalid_response};
			{error, _} = ErrUsername ->
				ErrUsername
		end,

		Cookie = snetd_auth:issue_token(#{
			id => <<Username/binary, "@itch.io">>
		}),
		redirect(Req, ~"/oauth_callback", [{~"data", Cookie}, {~"success", ~"1"}], State)
	else
		{error, game_not_owned} ->
			redirect(Req, ~"/oauth_callback", [{~"data", ~"game_not_owned"}, {~"success", ~"0"}], State);
		{error, _Reason} ->
			redirect(Req, ~"/oauth_callback", [{~"data", ~"internal_error"}, {~"success", ~"0"}], State)
	end;
init(Req, State) ->
	{ok, cowboy_req:reply(400, Req), State}.

%% Private

-spec itchio_api(iodata(), iodata()) -> {ok, map()} | {error, term()}.
itchio_api(Endpoint, Token) ->
	Url = uri_string:recompose(#{
		scheme => ~"https",
		host => ~"api.itch.io",
		path => Endpoint
	}),
	Headers = [{"authorization", [~"Bearer ", Token]}],
	maybe
		{ok, {{_HttpVer, Status, _}, _Headers, Body}} ?= httpc:request(get, {Url, Headers}, [], [{body_format, binary}], snetd_itchio),
		ok ?= case Status of
			200 -> ok;
			_ -> {error, {http, Body}}
		end,
		try json:decode(Body) of
			Result -> {ok, Result}
		catch
			error:_ ->
				{error, invalid_response}
		end
	end.

redirect(Req, Path, QsParams, Port) ->
	Qs = uri_string:compose_query(QsParams),
	RedirectUrl = uri_string:recompose(#{
		scheme => ~"http",
		host => ~"localhost",
		port => Port,
		path => Path,
		query => Qs
	}),
	{ok, cowboy_req:reply(302, #{~"location" => RedirectUrl}, Req), []}.
