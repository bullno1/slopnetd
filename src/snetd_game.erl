-module(snetd_game).
-behaviour(gen_server).
-export([
	start_link/2,
	info/1,
	server_info/1,
	make_join_token/1,
	decode_join_token/1,
	request_connect_token/2,
	request_join_permission/2,
	connect_webtransport/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([params/0, info/0]).
-include_lib("kernel/include/logger.hrl").

-define(PORT_CMD_REQUEST_CONNECT_TOKEN, 0).
-define(PORT_CMD_REQUEST_JOIN_PERMISSION, 1).
-define(PORT_CMD_WEBTRANSPORT_CONNECT, 2).
-define(PORT_CMD_WEBTRANSPORT_DISCONNECT, 3).
-define(PORT_CMD_WEBTRANSPORT_MESSAGE, 4).
-define(PORT_CMD_SERVER_INFO, 5).

-define(PORT_MSG_LOG, 0).
-define(PORT_MSG_QUERY_RESPONSE, 1).
-define(PORT_MSG_WEBTRANSPORT_SEND, 2).
-define(PORT_MSG_WEBTRANSPORT_DISCONNECT, 3).

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
	cn_server :: port(),
	wt_clients :: lproc_bimaps:bimap(non_neg_integer(), pid())
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

-spec server_info(gen_server:server_ref()) -> map().
server_info(Server) ->
	decode_server_info(gen_server:call(Server, server_info)).

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

-spec request_join_permission(binary(), binary()) -> ok | {error, binary()}.
request_join_permission(Game, Player) ->
	gen_server:call(
		{via, lproc, {?MODULE, Game}},
		{request_join_permission, Player}
	).

-spec connect_webtransport(binary(), binary()) -> {ok, port(), non_neg_integer()} | {error, binary()}.
connect_webtransport(Game, Player) ->
	gen_server:call(
		{via, lproc, {?MODULE, Game}},
		{connect_webtransport, Player}
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
	Addresses = snetd_utils:get_addresses(),
	CnServer = erlang:open_port(
		{spawn_executable, filename:join(code:priv_dir(slopnetd), "bin/cn_server")},
		[ {packet, 2}
		, {args, [
			"--created-by", UserId,
			"--connect-timeout-s", integer_to_binary(ConnectTimeoutS),
			"--max-num-players", integer_to_binary(MaxNumPlayers)
		  ] ++ lists:append([["--server-address",  inet:ntoa(Addr)] || Addr <- Addresses ])
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
		cn_server = CnServer,
		wt_clients = lproc_bimaps:new()
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
	server_info,
	_From,
	#state{cn_server = CnServer} = State
) ->
	port_command(CnServer, <<?PORT_CMD_SERVER_INFO>>),
	receive
		{CnServer, {data, <<?PORT_MSG_QUERY_RESPONSE, ServerInfoData/binary>>}} ->
			{reply, ServerInfoData, State}
	after 5000 ->
		exit(port_timeout)
	end;
handle_call(
	{request_connect_token, Username},
	_From,
	#state{cn_server = CnServer} = State
) ->
	port_command(CnServer, <<?PORT_CMD_REQUEST_CONNECT_TOKEN, Username/binary, 0>>),
	receive
		{CnServer, {data, <<?PORT_MSG_QUERY_RESPONSE, 0, Reason/binary>>}} ->
			{reply, {error, Reason}, State};
		{CnServer, {data, <<?PORT_MSG_QUERY_RESPONSE, 1, Token/binary>>}} ->
			{reply, {ok, Token}, State}
	after 5000 ->
		exit(port_timeout)
	end;
handle_call(
	{request_join_permission, Username},
	_From,
	#state{cn_server = CnServer} = State
) ->
	port_command(CnServer, <<?PORT_CMD_REQUEST_JOIN_PERMISSION, Username/binary, 0>>),
	receive
		{CnServer, {data, <<?PORT_MSG_QUERY_RESPONSE, 0, Reason/binary>>}} ->
			{reply, {error, Reason}, State};
		{CnServer, {data, <<?PORT_MSG_QUERY_RESPONSE, 1>>}} ->
			{reply, ok, State}
	after 5000 ->
		exit(port_timeout)
	end;
handle_call(
	{connect_webtransport, Username},
	{Pid, _Tag},
	#state{
		cn_server = CnServer,
		wt_clients = WtClients
	} = State
) ->
	port_command(CnServer, <<?PORT_CMD_WEBTRANSPORT_CONNECT, Username/binary, 0>>),
	receive
		{CnServer, {data, <<?PORT_MSG_QUERY_RESPONSE, 0>>}} ->
			{reply, {error, server_full}, State};
		{CnServer, {data, <<?PORT_MSG_QUERY_RESPONSE, 1, Slot/unsigned>>}} ->
			NewState = State#state{
				wt_clients = lproc_bimaps:add(Slot, Pid, WtClients)
			},
			monitor(process, Pid),
			{reply, {ok, CnServer, Slot}, NewState}
	after 5000 ->
		exit(port_timeout)
	end;
handle_call(Req, _From, State) ->
	?LOG_WARNING(#{ what => unknown_call, request => Req }),
	{noreply, State}.

handle_cast(_Req, State) ->
	{noreply, State}.

handle_info(
	{'DOWN', _MonitorRef, process, Pid, _Info},
	#state{ wt_clients = WtClients, cn_server = CnServer } = State
) ->
	case lproc_bimaps:take_by_value(Pid, WtClients) of
		{Slot, WtClients2} ->
			port_command(CnServer, <<?PORT_CMD_WEBTRANSPORT_DISCONNECT, Slot>>),
			{noreply, State#state{wt_clients = WtClients2}};
		error ->
			{noreply, State}
	end;
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

handle_port_message(
	<<?PORT_MSG_WEBTRANSPORT_SEND, Slot/unsigned, Reliable, Payload/binary>>,
	#state{ wt_clients = WtClients } = State
) ->
	case lproc_bimaps:find_by_key(Slot, WtClients) of
		{ok, Pid} ->
			Pid ! {?MODULE, message, Reliable, Payload};
		error ->
			ok
	end,
	State;
handle_port_message(
	<<?PORT_MSG_WEBTRANSPORT_DISCONNECT, Slot/unsigned>>,
	#state{ wt_clients = WtClients } = State
) ->
	case lproc_bimaps:find_by_key(Slot, WtClients) of
		{ok, Pid} ->
			Pid ! {?MODULE, disconnect};
		error ->
			ok
	end,
	State;
handle_port_message(<<?PORT_MSG_LOG, Log/binary>>, State) ->
	?LOG_DEBUG(#{ what => server_log, message => Log }),
	State.

decode_server_info(ServerInfoData) ->
	decode_server_info(ServerInfoData, #{}).

decode_server_info(<<>>, Map) ->
	Map;
decode_server_info(<<Id, Transport, NameLen, Name:NameLen/binary, Rest/binary>>, Acc) ->
	NewAcc = Acc#{
		Id => #{
			name => Name,
			transport => case Transport of
				0 -> cute_net;
				1 -> webtransport
			end
		}
	},
	decode_server_info(Rest, NewAcc).
