-module(snetd_game).
-behaviour(gen_server).
-export([
	start_link/2,
	info/1,
	make_join_token/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([params/0, info/0]).
-include_lib("kernel/include/logger.hrl").

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
	num_players = 0 :: non_neg_integer(),
	key_pair :: {cn_crypto:public_key(), cn_crypto:secret_key()},
	cn_server :: port(),
	connect_timeout_ms :: pos_integer(),
	shutdown_timer :: reference() | undefined
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

%% gen_server

init({
	UserId,
	#{ visibility := Visibility
	 , data := Data
	 } = Params
}) ->
	{ok, #{
		module := Module,
		connect_timeout_ms := ConnectTimeoutMs
	}} = application:get_env(slopnetd, game),
	ShutdownTimer = erlang:start_timer(ConnectTimeoutMs, self(), shutdown),

	CnServer = spawn_cn_server(Module),
	{PublicKey, SecretKey} = KeyPair = cn_crypto:make_keypair(),
	port_command(CnServer, <<PublicKey/binary, SecretKey/binary>>),

	_ = Visibility =:= public andalso lproc:subscribe(?MODULE, {UserId, Data}),
	{ok, #state{
		creator = UserId,
		params = Params,
		cn_server = CnServer,
		key_pair = KeyPair,
		connect_timeout_ms = ConnectTimeoutMs,
		shutdown_timer = ShutdownTimer
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
handle_call(Req, _From, State) ->
	?LOG_WARNING(#{ what => unknown_call, request => Req }),
	{noreply, State}.

handle_cast(_Req, State) ->
	{noreply, State}.

handle_info(
	{timeout, TimerRef, shutdown},
	#state{ shutdown_timer = TimerRef } = State
) ->
	{stop, shutdown, State};
handle_info(
	{Port, {exit_status, Status}},
	#state{ cn_server = Port } = State
) ->
	?LOG_WARNING(#{ what => server_terminated, status => Status }),
	{stop, shutdown, State};
handle_info(Msg, State) ->
	?LOG_WARNING(#{ what => unknown_message, message => Msg }),
	{noreply, State}.

%% Private

spawn_cn_server({Path, _Opts}) ->
	erlang:open_port(
		{spawn_executable, filename:join(code:priv_dir(slopnetd), "bin/cn_server")},
		[ {packet, 2}
		, {args, [resolve_path(Path)]}
		, exit_status
		, nouse_stdio
		%, overlapped_io
		, binary
		, hide
		]
	).

resolve_path(Path) when is_list(Path) ->
	Path;
resolve_path({priv_dir, App, RelPath}) ->
	filename:join(code:priv_dir(App), RelPath).
