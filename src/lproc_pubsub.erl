-module(lproc_pubsub).
-behaviour(gen_server).
% API
-export([start_link/0]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

% API

start_link() ->
    gen_server:start_link(
		{local, ?MODULE}, ?MODULE, [], [{hibernate_after, 5000}]
    ).

% gen_server

init([]) ->
    _ = tabkeeper:new(
		?MODULE,
		[
			ordered_set,
			protected,
			named_table,
			{keypos, 1},
			{read_concurrency, true}
		]
	),
	State = lists:foldl(
		fun({{Topic, Pid}, _}, Acc) ->
			maybe_start_monitor(Topic, Pid, Acc)
		end,
		lproc_bimaps:new(),
		ets:tab2list(?MODULE)
	),
    {ok, State}.

handle_call({subscribe, Topic, Extra}, {Pid, _}, State) ->
	ets:insert(?MODULE, {{Topic, Pid}, Extra}),
	{reply, ok, maybe_start_monitor(Topic, Pid, State)};
handle_call({unsubscribe, Topic}, {Pid, _}, State) ->
	Subscription = {Topic, Pid},
	ets:delete(?MODULE, Subscription),
	case lproc_bimaps:take_by_value(Subscription, State) of
		{MonitorRef, State2} ->
			demonitor(MonitorRef, [flush]),
			{reply, ok, State2};
		error ->
			{reply, ok, State}
	end;
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, _, _}, State) ->
	case lproc_bimaps:take_by_key(MonitorRef, State) of
		{Subscription, State2} ->
			ets:delete(?MODULE, Subscription),
			{noreply, State2};
		error ->
			{noreply, State}
	end;
handle_info(_, State) ->
    {noreply, State}.

% Private

maybe_start_monitor(Topic, Pid, State) ->
	Subscription = {Topic, Pid},
	case lproc_bimaps:has_value(Subscription, State) of
		true ->
			State;
		false ->
			MonitorRef = monitor(process, Pid),
			lproc_bimaps:add(MonitorRef, Subscription, State)
	end.
