-module(snetd_quic_acceptor_sup).
-behaviour(supervisor).
-export([start_link/4]).
-export([init/1]).

start_link(Module, Args, Listener, ListenHandle) ->
	supervisor:start_link(?MODULE, {Module, Args, Listener, ListenHandle}).

init({Module, Args, Listener, ListenHandle}) ->
	SupFlags = #{
		strategy => rest_for_one
	},
	ChildSpecs = [
		#{ id => conn_sup
		 , start => {snetd_quic_conn_sup, start_link, [Module, Args]}
		 , type => supervisor
		 },
		#{ id => acceptor
		 , start => {snetd_quic_acceptor, start_link, [self(), Listener, ListenHandle]}
		 , type => worker
		 }
	],
	{ok, {SupFlags, ChildSpecs}}.
