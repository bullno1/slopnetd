-module(snetd_quic_conn_sup).
-behaviour(supervisor).
-export([start_link/2]).
-export([init/1]).

start_link(Module, Args) ->
	supervisor:start_link(?MODULE, {Module, Args}).

init({Module, Args}) ->
	SupFlags = #{
		strategy => simple_one_for_one
	},
	ChildSpecs = [
		#{ id => connection
		 , start => {snetd_quic_connection, start_link, [self(), Module, Args]}
		 , modules => [Module]
		 , type => worker
		 , restart => temporary
		 }
	],
	{ok, {SupFlags, ChildSpecs}}.
