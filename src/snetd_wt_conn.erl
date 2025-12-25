-module(snetd_wt_conn).
-behaviour(cowboy_webtransport).
-export([routes/0]).
-export([
	init/2,
	webtransport_init/1,
	webtransport_handle/2,
	webtransport_info/2
]).
-include_lib("kernel/include/logger.hrl").
-include("snetd_port_commands.hrl").
-record(pending, {
	game :: binary(),
	username :: binary()
}).
-record(established, {
	port :: port(),
	slot :: non_neg_integer(),
	reliable_stream :: integer()
}).

-record(state, {
	server_conn :: #pending{} | #established{},
	reliable_buffer = ~"" :: binary(),
	idle_timeout_ms :: integer(),
	idle_timer :: reference(),
	last_message_timestamp_ms :: integer()
}).

%% Public

routes() ->
	[ {"/snet/game/wt", ?MODULE, []}
	].

%% cowboy_webtransport

init(#{ method := ~"CONNECT" } = Req, _) ->
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
	maybe
		{ok, GameName, Username} ?= case jwt:decode(Token, VerifyOptions) of
			{ok, #{ ~"game" := GameNameIn, ~"sub" := UsernameIn }} ->
				{ok, GameNameIn, UsernameIn};
			{error, _} ->
				{return, snetd_utils:reply_with_text(403, ~"invalid_token", Req)}
		end,
		Opts = #{ req_filter => fun (_) -> #{} end },
		{ok, #{ connect_timeout_s := IdleTimeoutS }} = application:get_env(slopnetd, game),
		IdleTimeoutMs = IdleTimeoutS * 1000,
		State = #state{
			server_conn = #pending{ game = GameName, username = Username },
			idle_timeout_ms = IdleTimeoutMs,
			idle_timer = erlang:start_timer(IdleTimeoutMs, self(), idle_timeout),
			last_message_timestamp_ms = erlang:monotonic_time(millisecond)
		},
		logger:update_process_metadata(#{ username => Username, game => GameName }),
		{cowboy_webtransport, Req, State, Opts}
	else
		EarlyReturn -> snetd_utils:handle_early_return(EarlyReturn, Req)
	end;
init(Req, _) ->
	{ok, cowboy_req:reply(404, Req), []}.

webtransport_init(State) ->
	{[{open_stream, reliable, bidi, []}], State}.

webtransport_handle(
	{opened_stream_id, reliable, StreamId},
	#state{
		server_conn = #pending{ game = GameName, username = Username }
	} = State
) ->
	% Only connect to the server after the reliable channel is setup so that if
	% the server send data immediately on connect, we are prepared
	case snetd_game:connect_webtransport(GameName, Username) of
		{ok, Port, Slot} ->
			NewState = State#state{
				server_conn = #established{
					port = Port,
					slot = Slot,
					reliable_stream = StreamId
				}
			},
			{[], NewState};
		{error, _} ->
			{[close], State}
	end;
webtransport_handle(
	{datagram, ~""},
	#state{} = State
) ->
	% Keep alive
	NewState = State#state{
		last_message_timestamp_ms = erlang:monotonic_time(millisecond)
	},
	{[], NewState};
webtransport_handle(
	{datagram, _Datagram},
	#state{ server_conn = #pending{} } = State
) ->
	?LOG_WARNING(#{ what => early_datagram }),
	{[], State};
webtransport_handle(
	{datagram, Datagram},
	#state{ server_conn = #established{ port = Port, slot = Slot } } = State
) ->
	port_command(Port, <<?PORT_CMD_WEBTRANSPORT_MESSAGE, Slot, Datagram/binary>>),
	NewState = State#state{
		last_message_timestamp_ms = erlang:monotonic_time(millisecond)
	},
	{[], NewState};
webtransport_handle(
	{stream_data, Stream, _, Data},
	#state{ server_conn = #established{ reliable_stream = Stream } } = State
) ->
	{[], process_reliable(Data, State)};
webtransport_handle(Event, State) ->
	?LOG_WARNING(#{ what => ignored_event, event => Event }),
	{[], State}.

webtransport_info(
	{timeout, TimerRef, idle_timeout},
	#state{
		idle_timer = TimerRef,
		idle_timeout_ms = IdleTimeoutMs,
		last_message_timestamp_ms = LastMessageTimestampMs
	} = State
) ->
	CurrentTimeMs = erlang:monotonic_time(millisecond),
	case (CurrentTimeMs - LastMessageTimestampMs) >= IdleTimeoutMs of
		true ->
			{[close], State};
		false ->
			NewState = State#state{
				idle_timer = erlang:start_timer(IdleTimeoutMs, self(), idle_timeout)
			},
			{[], NewState}
	end;
webtransport_info({snetd_game, message, 0, Msg}, State) ->
	{[{send, datagram, Msg}], State};
webtransport_info(
	{snetd_game, message, 1, Msg},
	#state{ server_conn = #established{ reliable_stream = Stream } } = State
) ->
	MsgLen = byte_size(Msg),
	{[{send, Stream, [<<MsgLen:16/unsigned-little>>, Msg]}], State};
webtransport_info({snetd_game, disconnect}, State) ->
	{[close], State};
webtransport_info(Msg, State) ->
	?LOG_WARNING(#{ what => unknown_message, message => Msg }),
	{[], State}.

%% Private

process_reliable(
	Chunk,
	#state{ server_conn = Conn, reliable_buffer = Buffer } = State
) ->
	LeftOver = process_reliable1(<<Buffer/binary, Chunk/binary>>, Conn),
	State#state{ reliable_buffer = LeftOver }.

process_reliable1(
	<<Length:16/unsigned-little, Msg:Length/binary, Rest/binary>>,
	#established{ port = Port, slot = Slot } = Conn
) ->
	port_command(Port, <<?PORT_CMD_WEBTRANSPORT_MESSAGE, Slot, Msg/binary>>),
	process_reliable1(Rest, Conn);
process_reliable1(LeftOver, _Conn) ->
	LeftOver.
