-module(snetd_lobby).
-behaviour(cowboy_handler).
-export([routes/0]).
-export([init/2]).
-include_lib("kernel/include/logger.hrl").

%% Public

routes() ->
	[ {"/snet/game/create", ?MODULE, create}
	, {"/snet/game/list", ?MODULE, list}
	, {"/snet/game/join", ?MODULE, join}
	].

%% cowboy_handler

init(Req, create) ->
	maybe
		{ok, #{ id := UserId }} ?= case snetd_auth:auth_req(Req) of
			{ok, _} = AuthOk -> AuthOk;
			{error, _} -> {return, cowboy_req:reply(401, Req)}
		end,
		BodyOpts = #{ length => 2048, period => 5000 },
		{ok, Body, Req2} ?= case cowboy_req:read_body(Req, BodyOpts) of
			{ok, _, _} = ReadBody ->
				ReadBody;
			{more, _, Req2In} ->
				{return, cowboy_req:reply(400, Req2In)}
		end,
		{ok, GameParams} ?= try json:decode(Body) of
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
		catch
			error:_ ->
				{return, cowboy_req:reply(400, Req2)}
		end,
		{ok, GamePid} ?= snetd_game_sup:start_game(UserId, GameParams),
		GameInfo = snetd_game:info(GamePid),
		{ ok
		, cowboy_req:reply(
			200,
			#{~"content-type" => ~"application/json" },
			json:encode(GameInfo),
			Req2
		  )
		, []
		}
	else
		{return, FinalReq} ->
			{ok, FinalReq, []};
		{error, Error} ->
			?LOG_ERROR(#{ error => Error }),
			{ok, cowboy_req:reply(500, Req), []}
	end;
init(Req, State) ->
	{ok, cowboy_req:reply(400, Req), State}.
