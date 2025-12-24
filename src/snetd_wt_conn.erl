-module(snetd_wt_conn).
-behaviour(cowboy_webtransport).
-export([routes/0]).
-export([
	init/2,
	webtransport_handle/2,
	webtransport_info/2
]).

%% Public

routes() ->
	[ {"/snet/game/wt", ?MODULE, []}
	].

%% cowboy_webtransport

init(#{ method := ~"CONNECT" } = Req, _) ->
	io:format("~p~n", [Req]),
	#{ token := Token } = cowboy_req:match_qs([{token, nonempty}], Req),
	{ok, #{ verify_algorithms := Algorithms }} = application:get_env(slopnetd, jwt),
	#{ keys := Keys } = keymaker:info({slopnetd, jwt}),
	VerifyOptions = #{
		keys => Keys,
		algorithms => Algorithms,
		validators => [
			jwt:validate_sub(),
			jwt:validate_exp(),
			{ ~"game", fun is_binary/1 }
		]
	},
	case jwt:decode(Token, VerifyOptions) of
		{ok, #{ ~"game" := GameName, ~"sub" := Username }} ->
			Opts = #{ req_filter => fun (_) -> #{} end },
			{cowboy_webtransport, Req, {GameName, Username}, Opts};
		{error, _} ->
			snetd_utils:reply_with_text(403, ~"invalid_token", Req)
	end;
init(Req, _) ->
	{ok, cowboy_req:reply(404, Req), []}.

webtransport_handle(Event, State) ->
	io:format("~p~n", [Event]),
	{[], State}.

webtransport_info(_, State) ->
	{[], State}.
