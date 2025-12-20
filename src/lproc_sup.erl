-module(lproc_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

% API

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% supervisor

init([]) ->
	Procs = [
		#{id => lproc_name, start => {lproc_name, start_link, []}},
		#{id => lproc_pubsub, start => {lproc_pubsub, start_link, []}}
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.
