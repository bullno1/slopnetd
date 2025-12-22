-module(snetd_game).
-behaviour(gen_server).
-export([
	start_link/2,
	info/1,
	make_join_token/1,
	decode_join_token/1,
	request_connect_token/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([params/0, info/0]).
-include_lib("kernel/include/logger.hrl").

-define(PORT_CMD_REQUEST_CONNECT_TOKEN, 0).

-define(PORT_MSG_LOG, 0).
-define(PORT_MSG_CONNECT_TOKEN, 1).

-type params() :: #{
	visibility := public | private,
	max_num_players := non_neg_integer(),
	data := binary()
}.
-type info() :: #{
	join_token := binary(),
	creator := binary(),
	data := binary()
}.

-record(state, {
	creator :: binary(),
	params :: params(),
	cn_server :: port()
}).

%% Public

start_link(UserId, Params) ->
	gen_server:start_link(
		{via, lproc, {?MODULE, UserId}},
		?MODULE, {UserId, Params},
		[{hibernate_after, 10000}]
	).

-spec info(gen_server:server_ref()) -> info().
info(Server) ->
	gen_server:call(Server, info).

-spec make_join_token(binary()) -> binary().
make_join_token(GameName) ->
	{ok, #{ join_token_ttl_s := TTL }} = application:get_env(slopnetd, game),
	{ok, #{ signing_algorithm := SigningAlg }} = application:get_env(slopnetd, jwt),
	#{ preferred_key := {Kid, Key} } = keymaker:info({slopnetd, jwt}),
	Claims = #{
		~"game" => GameName,
		~"exp" => erlang:system_time(second) + TTL
	},
	SigningOpts = #{
		kid => Kid,
		key => Key,
		algorithm => SigningAlg
	},
	jwt:issue(Claims, SigningOpts).

-spec decode_join_token(binary()) -> {ok, map()} | {error, term()}.
decode_join_token(Token) ->
	{ok, #{ verify_algorithms := Algorithms }} = application:get_env(slopnetd, jwt),
	#{ keys := Keys } = keymaker:info({slopnetd, jwt}),
	VerifyOptions = #{
		keys => Keys,
		algorithms => Algorithms,
		validators => [
			jwt:validate_exp(),
			{ ~"game", fun is_binary/1 }
		]
	},
	jwt:decode(Token, VerifyOptions).

-spec request_connect_token(binary(), binary()) -> {ok, binary()} | {error, binary()}.
request_connect_token(Game, Player) ->
	gen_server:call(
		{via, lproc, {?MODULE, Game}},
		{request_connect_token, Player}
	).

%% gen_server

init({
	UserId,
	#{ visibility := Visibility
	 , data := Data
	 , max_num_players := MaxNumPlayers
	 } = Params
}) ->
	{ok, #{
		module := {ModulePath, _ModuleExtra},
		connect_timeout_s := ConnectTimeoutS
	}} = application:get_env(slopnetd, game),


	{ok, Addresses} = inet:getifaddrs(),
	Addrs = [proplists:get_value(addr, Props) || {_Interface, Props} <- Addresses],

	CnServer = erlang:open_port(
		{spawn_executable, filename:join(code:priv_dir(slopnetd), "bin/cn_server")},
		[ {packet, 2}
		, {args, [
			"--created-by", UserId,
			"--connect-timeout-s", integer_to_binary(ConnectTimeoutS),
			"--max-num-players", integer_to_binary(MaxNumPlayers)
		  ] ++ lists:append([["--server-address",  inet:ntoa(Addr)] || Addr <- Addrs ])
		    ++ [resolve_module_path(ModulePath)]
		  }
		, exit_status
		, nouse_stdio
		%, overlapped_io
		, binary
		, hide
		]
	),
	_ = Visibility =:= public andalso lproc:subscribe(?MODULE, {UserId, Data}),
	{ok, #state{
		creator = UserId,
		params = Params,
		cn_server = CnServer
	}}.

handle_call(
	info,
	_From,
	#state{
		creator = Creator,
		params = #{ data := Data }
	} = State
) ->
	Result = #{
		join_token => make_join_token(Creator),
		creator => Creator,
		data => Data
	},
	{reply, Result, State};
handle_call(
	{request_connect_token, Username},
	_From,
	#state{cn_server = CnServer} = State
) ->
	port_command(CnServer, <<?PORT_CMD_REQUEST_CONNECT_TOKEN, Username/binary, 0>>),
	receive
		{CnServer, {data, <<?PORT_MSG_CONNECT_TOKEN, 0, Reason/binary>>}} ->
			{reply, {error, Reason}, State};
		{CnServer, {data, <<?PORT_MSG_CONNECT_TOKEN, 1, Token/binary>>}} ->
			{reply, {ok, Token}, State}
	after 5000 ->
		exit(port_timeout)
	end;
handle_call(Req, _From, State) ->
	?LOG_WARNING(#{ what => unknown_call, request => Req }),
	{noreply, State}.

handle_cast(_Req, State) ->
	{noreply, State}.

handle_info({Port, {data, PortMessage}}, #state{ cn_server = Port } = State) ->
	{noreply, handle_port_message(PortMessage, State)};
handle_info(
	{Port, {exit_status, Status}},
	#state{ cn_server = Port } = State
) ->
	?LOG_INFO(#{ what => server_terminated, status => Status }),
	{stop, shutdown, State};
handle_info(Msg, State) ->
	?LOG_WARNING(#{ what => unknown_message, message => Msg }),
	{noreply, State}.

%% Private

resolve_module_path(Path) when is_list(Path) ->
	Path;
resolve_module_path({priv_dir, App, RelPath}) ->
	filename:join(code:priv_dir(App), RelPath).

handle_port_message(<<?PORT_MSG_LOG, Log/binary>>, State) ->
	?LOG_DEBUG(#{ what => server_log, message => Log }),
	State.
