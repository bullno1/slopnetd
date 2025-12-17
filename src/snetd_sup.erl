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
	ChildSpecs = [],
	{ok, {SupFlags, ChildSpecs}}.
