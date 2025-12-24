-module(snetd_quic).
-behaviour(supervisor).
-export([
	start_link/3,
	start_link/4,
	reload/2
]).
-export([init/1]).
-export_type([opts/0, listen_opts/0, acceptor_opts/0]).
-include_lib("quicer/include/quicer_types.hrl").

-type opts() :: #{
	listen_port := inet:port_number(),
	listen_opts => map(),  % quicer's type is all over the place and wrong
	accept_opts => acceptor_opts(),
	num_acceptors => pos_integer()
}.

%% Public

-spec start_link(module(), term(), opts()) -> gen_server:start_ret().
start_link(Module, Args, Opts) ->
	supervisor:start_link(?MODULE, {Module, Args, Opts}).

-spec start_link(supervisor:sup_name(), module(), term(), opts()) -> gen_server:start_ret().
start_link(Name, Module, Args, Opts) ->
	supervisor:start_link(Name, ?MODULE, {Module, Args, Opts}).

-spec reload(supervisor:sup_ref(), opts()) -> ok.
reload(Ref, NewOpts) ->
	case supervisor:which_child(Ref, listener) of
		{ok, {_, Listener, _, _}} when is_pid(Listener) ->
			gen_server:call(Listener, {reload, NewOpts});
		{ok, {_, _Listener, _, _}} ->
			{error, listener_down};
		{error, _} = Err ->
			Err
	end.

%% Supervisor

init({Module, Args, Opts}) ->
	SupFlags = #{
		strategy => one_for_all
	},
	ChildSpecs = [
		#{ id => acceptor_sup
		 , start => {snetd_quic_acceptor_sup_sup, start_link, [Module, Args]}
		 , type => supervisor
		 },
		#{ id => listener
		 , start => {snetd_quic_listener, start_link, [self(), Opts]}
		 , type => worker
		 }
	],
	{ok, {SupFlags, ChildSpecs}}.
