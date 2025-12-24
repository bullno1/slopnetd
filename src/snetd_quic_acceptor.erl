-module(snetd_quic_acceptor).
-behaviour(sys).
-export([start_link/3]).
-export([
	system_continue/3,
	system_terminate/4,
	system_code_change/4,
	system_get_state/1,
	system_replace_state/2
]).
-export([init/3]).

-record(state, {
	parent :: pid(),
	conn_sup :: pid(),
	accept_opts :: snetd_quic:acceptor_opts(),
	listener :: pid(),
	listen_handle :: quicer:listener_handle()
}).

start_link(Parent, Listener, ListenHandle) ->
	proc_lib:start_link(?MODULE, init, [Parent, Listener, ListenHandle]).

init(Parent, Listener, ListenHandle) ->
	proc_lib:init_ack(Parent, {ok, self()}),
	ConnSup = case supervisor:which_child(Parent, conn_sup) of
		{ok, {_, ConnSupPid, _, _}} when is_pid(ConnSupPid) ->
			ConnSupPid;
		{ok, _} ->
			exit({conn_sup, down});
		{error, ConnSupErr} ->
			exit({conn_sup, ConnSupErr})
	end,

	AcceptOpts = gen_server:call(Listener, get_accept_opts),
	State = #state{
		parent = Parent,
		conn_sup = ConnSup,
		accept_opts = AcceptOpts,
		listener = Listener,
		listen_handle = ListenHandle
	},
	accept_loop(State).

%% sys

system_continue(_Parent, _Debug, State) ->
	wait_loop(State).

system_terminate(Reason, _Parent, _Debug, _State) ->
	exit(Reason).

system_code_change(State, _Module, _OldVsn, _Extra) ->
	{ok, State}.

system_get_state(#state{ accept_opts = Opts }) ->
	{ok, Opts}.

system_replace_state(StateFun, #state{ accept_opts = Opts } = State) ->
	NewOpts = StateFun(Opts),
	{ok, NewOpts, State#state{ accept_opts = NewOpts }}.

%% Private

accept_loop(#state{listen_handle = ListenHandle, accept_opts = AcceptOpts} = State) ->
	case quicer:async_accept(ListenHandle, AcceptOpts) of
		{ok, _} ->
			wait_loop(State);
		{error, AcceptErr} ->
			exit({accept_err, AcceptErr})
	end.

wait_loop(State) ->
	receive
		{quic, new_conn, Conn, _} ->
			handle_connection(Conn, State);
		{quic, connected, Conn, _} ->
			handle_connection(Conn, State);
		{system, From, Msg} ->
			sys:handle_system_msg(Msg, From, State#state.parent, ?MODULE, [], State)
	end.

handle_connection(Conn, #state{ conn_sup = ConnSup } = State) ->
	case supervisor:start_child(ConnSup, []) of
		{ok, Handler} ->
			quicer:controlling_process(Conn, Handler),
			Handler ! {start, Conn};
		{error, _} ->
			ok
	end,
	accept_loop(State).
