-module(snetd_quic_listener).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2]).
-include_lib("kernel/include/logger.hrl").

-record(state, {
	opts :: snetd_quic:opts(),
	acceptor_sup :: pid() | undefined,
	listen_handle :: quicer:listener_handle()
}).

%% Public

-spec start_link(pid(), snetd_quic:opts()) -> gen_server:start_ret().
start_link(Parent, Opts) ->
	gen_server:start_link(?MODULE, {Parent, Opts}, [{hibernate_after, 1000}]).

%% gen_server

-dialyzer({nowarn_function, init/1}).
init({Parent, Opts}) ->
	case quicer:listen(
		maps:get(listen_port, Opts),
		maps:get(listen_opts, Opts, #{})
	) of
		{ok, ListenHandle} ->
			State = #state{
				opts = Opts,
				listen_handle = ListenHandle
			},
			{ok, State, {continue, {spawn_acceptors, Parent}}};
		{error, ListenError} ->  % quicer has the wrong type here
			{stop, {quic_error, ListenError}};
		{error, Class, Detail} ->
			{stop, {quic_error, {Class, Detail}}}
	end.

handle_call(
	{reload, NewOpts},
	_From,
	#state{ listen_handle = ListenHandle, acceptor_sup = AcceptorSup } = State
) ->
	maybe
		ok ?= case quicer:stop_listener(ListenHandle) of
			ok -> ok;
			{error, _} = StopErr ->
				{noreply, {stop, StopErr}, State}
		end,
		NewPort = maps:get(listen_port, NewOpts),
		NewListenOpts = maps:get(listen_opts, NewOpts, #{}),
		_ = [
			restart_acceptor(Child)
			|| {_, Child, _, _} <- supervisor:which_children(AcceptorSup), is_pid(Child)
		],
		ok ?= case quicer:start_listener(ListenHandle, NewPort, NewListenOpts) of
			ok -> ok;
			{error, _} = StartErr ->
				{noreply, {stop, StartErr}, State}
		end,
		{reply, ok, State#state{opts = NewOpts}}
	end;
handle_call(get_accept_opts, _From, #state{ opts = Opts } = State) ->
	{reply, maps:get(accept_opts, Opts, #{}), State};
handle_call(Req, _From, State) ->
	?LOG_WARNING(#{ what => unknown_call, request => Req }),
	{noreply, State}.

handle_cast(Req, State) ->
	?LOG_WARNING(#{ what => unknown_cast, request => Req }),
	{noreply, State}.

handle_continue(
	{spawn_acceptors, Sup},
	#state{ listen_handle = ListenHandle, opts = Opts } = State
) ->
	maybe
		{ok, AcceptorSup} ?= case supervisor:which_child(Sup, acceptor_sup) of
			{ok, {_, AcceptorSupPid, _, _}} when is_pid(AcceptorSupPid) ->
				{ok, AcceptorSupPid};
			{ok, _} ->
				{stop, {acceptor, down}};
			{error, AcceptorSupErr} ->
				{stop, {acceptor, AcceptorSupErr}}
		end,
		StartAcceptorsResult = lists:foldl(
			fun (_, ok) ->
				case supervisor:start_child(AcceptorSup, [self(), ListenHandle]) of
					{ok, _} -> ok;
					{ok, _, _} -> ok;
					{error, _} = Err -> Err
				end;
				(_, {error, _} = Err) ->
					Err
			end,
			ok,
			lists:seq(1, maps:get(num_acceptors, Opts, 1))
		),
		ok ?= case StartAcceptorsResult of
			ok -> ok;
			{error, AcceptorErr} ->
				{stop, {acceptor, AcceptorErr}}
		end,
		{noreply, State#state{ acceptor_sup = AcceptorSup }}
	end.

restart_acceptor(Acceptor) ->
	supervisor:restart_child(Acceptor, acceptor).
