-module(snetd_lobby).
-behaviour(cowboy_handler).
-export([routes/0]).
-export([init/2]).

%% Public

routes() ->
	[ {"/snet/game/create", ?MODULE, create}
	, {"/snet/game/list", ?MODULE, list}
	, {"/snet/game/join", ?MODULE, join}
	].

%% cowboy_handler

init(#{ method := ~"POST" } = Req, create) ->
	maybe
		{ok, #{ id := UserId }} ?= snetd_auth:auth_req(Req),
		BodyOpts = #{ length => 1024, period => 5000 },
		{ok, Body, Req2} ?= snetd_utils:read_body_json(Req, BodyOpts),
		{ok, GameParams} ?= case Body of
			#{ ~"visibility" := Visibility
			 , ~"max_num_players" := MaxNumPlayers
			 } = GameParamsIn when
				  (is_integer(MaxNumPlayers) andalso MaxNumPlayers > 0)
				, (Visibility =:= ~"public" orelse Visibility =:= ~"private") ->
					Data = maps:get(~"data", GameParamsIn, ~""),
				case is_binary(Data) of
					true ->
						{ok, #{
							max_num_players => MaxNumPlayers,
							visibility => binary_to_existing_atom(Visibility),
							data => Data
						}};
					false ->
						{return, cowboy_req:reply(400, Req2)}
				end;
			_ ->
				{return, cowboy_req:reply(400, Req2)}
		end,
		{ok, GamePid} ?= snetd_game_sup:start_game(UserId, GameParams),
		GameInfo = snetd_game:info(GamePid),
		{ ok
		, snetd_utils:reply_with_json(200, GameInfo, Req2)
		, []
		}
	else
		Return -> snetd_utils:handle_early_return(Return, Req)
	end;
init(#{ method := ~"POST" } = Req, join) ->
	maybe
		#{ transport := Transport } = cowboy_req:match_qs([{transport, nonempty}], Req),
		{ok, #{ id := UserId }} ?= snetd_auth:auth_req(Req),
		BodyOpts = #{ length => 1024, period => 5000 },
		{ok, Body, Req2} ?= snetd_utils:read_body(Req, BodyOpts),
		{ok, GameName} ?= case snetd_game:decode_join_token(Body) of
			{ok, #{ ~"game" := GameNameIn }} -> {ok, GameNameIn};
			{error, _} -> {return, snetd_utils:reply_with_text(400, ~"invalid_token", Req2)}
		end,

		{ok, ConnectToken} ?= case Transport of
			~"cute_net" ->
				case snetd_game:request_connect_token(GameName, UserId) of
					{ok, _} = Granted -> Granted;
					{error, Reason} ->
						{return, snetd_utils:reply_with_text(403, Reason, Req2)}
				end;
			~"webtransport" ->
				case ok of
					ok ->
						{ok, generate_webtransport_token(GameName, UserId)};
					{error, Reason} ->
						{return, snetd_utils:reply_with_text(403, Reason, Req2)}
				end;
			_ ->
				{return, snetd_utils:reply_with_text(400, ~"invalid_transport", Req2)}
		end,
		{ ok
		, cowboy_req:reply(
			200,
			#{ ~"content-type" => ~"application/octet-stream" },
			ConnectToken,
			Req2
		  )
		, []
		}
	else
		Return -> snetd_utils:handle_early_return(Return, Req)
	end;
init(#{ method := ~"GET" } = Req, list) ->
	maybe
		{ok, #{ id := _ }} ?= snetd_auth:auth_req(Req),
		Games = [Data || {_Pid, Data} <- lproc:list(snetd_game)],
		Entries = [
			#{ join_token => snetd_game:make_join_token(GameName)
			 , creator => GameName
			 , data => ~""
			 } || {GameName, _Data} <- Games
		],
		{ ok
		, snetd_utils:reply_with_json(
			200,
			#{ games => Entries },
			Req
		  )
		, []
		}
	else
		Return -> snetd_utils:handle_early_return(Return, Req)
	end;
init(#{ method := ~"OPTIONS" } = Req, _) ->
	CorsOpts = #{ allowed_methods => [~"POST"] },
	{ok, snetd_utils:cors(Req, CorsOpts), []};
init(Req, State) ->
	{ok, cowboy_req:reply(400, Req), State}.

%% Private

generate_webtransport_token(GameName, UserId) ->
	{ok, #{
		connect_timeout_s := ConnectTimeoutS
	}} = application:get_env(slopnetd, game),
	{ok, #{ signing_algorithm := SigningAlg }} = application:get_env(slopnetd, jwt),
	#{ preferred_key := {Kid, Key} } = keymaker:info({slopnetd, jwt}),
	Claims = #{
		~"sub" => UserId,
		~"game" => GameName,
		~"exp" => erlang:system_time(second) + ConnectTimeoutS
	},
	SigningOpts = #{
		kid => Kid,
		key => Key,
		algorithm => SigningAlg
	},
	AuthToken = jwt:issue(Claims, SigningOpts),
	Qs = uri_string:compose_query([{~"token", AuthToken}]),

	{ok, #{ listen_port := QuicPort }} = application:get_env(slopnetd, quic),
	ConnectUrls = [
		iolist_to_binary(uri_string:recompose(#{
			scheme => ~"https",
			host => inet:ntoa(Address),
			port => QuicPort,
			path => ~"/snet/game/wt",
			query => Qs
		}))
		|| Address <- snetd_utils:get_addresses()
	],

	#{ keys := Keys } = keymaker:info({slopnetd, quic}),
	CertHashes = [
		base64:encode(crypto:hash(sha256, CertDer))
		|| _KeyId := {CertDer, _KeyDer} <- Keys
	],
	json:encode(#{
		~"urls" => ConnectUrls,
		~"serverCertificateHashes" => CertHashes
	}).
