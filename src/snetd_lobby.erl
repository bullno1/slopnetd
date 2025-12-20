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

init(Req, create) ->
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
init(Req, State) ->
	{ok, cowboy_req:reply(400, Req), State}.
