-module(snetd_quic_connection).
-export([start_link/3]).
-export([init/3]).

-callback init(Parent, Args, Conn) -> no_return() when
	Parent :: pid(),
	Args :: term(),
	Conn :: quicer:connection_handle().

start_link(Parent, Module, Args) ->
	proc_lib:start_link(?MODULE, init, [Parent, Module, Args]).

init(Parent, Module, Args) ->
	proc_lib:init_ack(Parent, {ok, self()}),
	receive
		{start, Conn} ->
			quicer:handshake(Conn),
			Module:init(Parent, Args, Conn)
	after 5000 ->
		exit(timeout)
	end.
