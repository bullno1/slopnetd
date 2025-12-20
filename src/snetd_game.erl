-module(snetd_game).
-behaviour(gen_server).
-export([start_link/2, info/1]).
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
	%num_players = 0 :: non_neg_integer(),
	join_timeout :: pos_integer(),
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

%% gen_server

init({
	UserId,
	#{ visibility := Visibility
	 , data := Data
	 } = Params
}) ->
	{ok, #{
		join_timeout := JoinTimeout
	}} = application:get_env(slopnetd, game),
	ShutdownTimer = erlang:start_timer(JoinTimeout, self(), shutdown),

	_ = Visibility =:= public andalso lproc:subscribe(?MODULE, {UserId, Data}),
	{ok, #state{
		creator = UserId,
		params = Params,
		join_timeout = JoinTimeout,
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
	{reply, #{ join_token => ~"", creator => Creator, data => Data }, State};
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
handle_info(_, State) ->
	{noreply, State}.
