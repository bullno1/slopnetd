-module(snetd_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	SupFlags = #{
		strategy => one_for_one
	},
	ChildSpecs = [
		#{id => tabkeeper, start => {tabkeeper, start_link, []}},
		#{id => lproc_sup, start=> {lproc_sup, start_link, []}},
		snetd_game_sup:child_spec()
	],
	{ok, {SupFlags, ChildSpecs}}.
