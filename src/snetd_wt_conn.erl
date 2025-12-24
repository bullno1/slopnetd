-module(snetd_wt_conn).
-behaviour(cowboy_webtransport).
-export([routes/0]).
-export([
	init/2,
	webtransport_handle/2,
	webtransport_info/2
]).
-record(state, {
	port :: port(),
	slot :: non_neg_integer()
}).
-include_lib("kernel/include/logger.hrl").

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
		{ok, Port, Slot} ?= case snetd_game:connect_webtransport(GameName, Username) of
			{ok, _, _} = Connected ->
				Connected;
			{error, _} ->
				{return, snetd_utils:reply_with_text(403, ~"server_full", Req)}
		end,
		Opts = #{ req_filter => fun (_) -> #{} end },
		State = #state{
			port = Port,
			slot = Slot
		},
		{cowboy_webtransport, Req, State, Opts}
	else
		EarlyReturn -> snetd_utils:handle_early_return(EarlyReturn, Req)
	end;
init(Req, _) ->
	{ok, cowboy_req:reply(404, Req), []}.

webtransport_handle(
	{datagram, Datagram},
	#state{ port = Port, slot = Slot} = State
) ->
	port_command(Port, <<4, Slot, Datagram/binary>>),
	{[], State};
webtransport_handle(Event, State) ->
	?LOG_WARNING(#{ what => ignored_event, event => Event }),
	{[], State}.

webtransport_info({snetd_game, message, 0, Msg}, State) ->
	{[{send, datagram, Msg}], State};
webtransport_info({snetd_game, disconnect}, State) ->
	{[close], State};
webtransport_info(Msg, State) ->
	?LOG_WARNING(#{ what => unknown_message, message => Msg }),
	{[], State}.
