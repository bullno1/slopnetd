-module(snetd_auth_itchio).
-behaviour(cowboy_handler).
-export([routes/0]).
-export([init/2]).

%% Public

routes() ->
	[ {"/snet/auth/itchio/start", snetd_auth_itchio, start}
	, {"/snet/auth/itchio/callback", cowboy_static, {priv_file, slopnetd, "www/itchio/callback.html"}}
	, {"/snet/auth/itchio/callback2", snetd_auth_itchio, callback}
	, {"/snet/auth/itchio/end", cowboy_static, {priv_file, slopnetd, "www/itchio/end.html"}}
	, {"/snet/auth/itchio/[...]", cowboy_static, {priv_dir, slopnetd, "www/itchio/assets"}}
	].

%% cowboy_handler

init(Req, start) ->
	#{ origin := Origin } = cowboy_req:match_qs([{origin, nonempty}], Req),
	{ok, #{
		client_id := ClientId,
		redirect_uri := RedirectUri
	}} = application:get_env(slopnetd, itchio),
	Qs = uri_string:compose_query([
		{~"client_id", ClientId},
		{~"redirect_uri", RedirectUri},
		{~"response_type", ~"token"},
		{~"scope", ~"profile:me game:view:ownership"},
		{~"state", Origin}
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
		state := Origin
	} = cowboy_req:match_qs([{access_token, nonempty}, {state, nonempty}], Req),
	{ok, #{
		game_id := GameId,
		allowed_origins := AllowedOrigins
	}} = application:get_env(slopnetd, itchio),
	maybe
		ok ?= case Origin of
			<<"http://localhost:", _/binary>> ->
				ok;
			OtherOrigin ->
				case lists:member(OtherOrigin, AllowedOrigins) of
					true -> ok;
					false ->
						{return, snetd_utils:reply_with_text(400, ~"Invalid origin", Req)}
				end
		end,
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
		finish(Req, [{~"data", Cookie}, {~"success", ~"1"}], Origin)
	else
		{error, game_not_owned} ->
			finish(Req, [{~"data", ~"game_not_owned"}, {~"success", ~"0"}], Origin);
		{error, _Reason} ->
			finish(Req, [{~"data", ~"internal_error"}, {~"success", ~"0"}], Origin);
		EarlyReturn ->
			snetd_utils:handle_early_return(EarlyReturn, Req)
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

finish(Req, QsParams, <<"http://localhost:", _/binary>> = Origin) ->
	Qs = uri_string:compose_query(QsParams),
	{ ok
	, cowboy_req:reply(
		302,
		#{~"location" => <<Origin/binary, "/oauth_callback", $?, Qs/binary>>},
		Req
	  )
	, []
	};
finish(Req, QsParams, Origin) ->
	{ok, Content} = snetd_auth_itchio_return_dtl:render(#{
		data => json:encode(#{
			result => maps:from_list(QsParams),
			origin => Origin
		})
	}),
	{ ok
	, cowboy_req:reply(
		200,
		#{~"content-type" => ~"text/html"},
		Content,
		Req
	  )
	, []
	}.
