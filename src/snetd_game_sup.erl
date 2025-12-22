-module(snetd_game_sup).
-behaviour(supervisor).
-export([
	start_link/0,
	child_spec/0,
	start_game/2
]).
-export([init/1]).

%% Public

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
	#{
		id => ?MODULE,
		start => {?MODULE, start_link, []},
		type => supervisor
	}.

-spec start_game(binary(), snetd_game:params()) -> {ok, pid()} | {error, term()}.
start_game(UserId, Params) ->
	case supervisor:start_child(?MODULE, [UserId, Params]) of
		{ok, _} = Started -> Started;
		{error, {already_started, Pid}} -> {ok, Pid};
		{error, _} = Error -> Error
	end.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor

init([]) ->
	SupFlags = #{ strategy => simple_one_for_one },
	ChildSpecs = [#{
		id => snetd_game,
		start => {snetd_game, start_link, []},
		restart => temporary
	}],
	{ok, {SupFlags, ChildSpecs}}.
