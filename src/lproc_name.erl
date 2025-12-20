-module(lproc_name).
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
			set,
			protected,
			named_table,
			{keypos, 1},
			{read_concurrency, true}
		]
	),
	State = lists:foldl(
		fun({Name, Pid}, Acc) ->
			start_monitor(Name, Pid, Acc)
		end,
		lproc_bimaps:new(),
		ets:tab2list(?MODULE)
	),
    {ok, State}.

handle_call({register_name, Name, PidOrPort}, _, State) ->
    case ets:insert_new(?MODULE, {Name, PidOrPort}) of
        true ->
            {reply, yes, start_monitor(Name, PidOrPort, State)};
        false ->
			PidOrPort = lproc:whereis_name(Name),
            case is_alive(PidOrPort) of
				true ->
					{reply, no, State};
				false ->
					State2 = stop_monitor(Name, State),
					ets:insert(?MODULE, {Name, PidOrPort}),
					{reply, yes, start_monitor(Name, PidOrPort, State2)}
			end
    end;
handle_call({unregister_name, Name}, _, State) ->
	ets:delete(?MODULE, Name),
	{reply, ok, stop_monitor(Name, State)};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, _, _, _}, State) ->
	case lproc_bimaps:take_by_key(MonitorRef, State) of
        {Name, State2} ->
            ets:delete(?MODULE, Name),
            {noreply, State2};
        error ->
			{noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

% Private

start_monitor(Name, PidOrPort, State) ->
	MonitorRef = case PidOrPort of
		Pid when is_pid(Pid) -> monitor(process, Pid);
		Port when is_port(Port) -> monitor(port, Port)
	end,
	lproc_bimaps:add(MonitorRef, Name, State).

stop_monitor(Name, State) ->
	case lproc_bimaps:take_by_value(Name, State) of
		{MonitorRef, State2} ->
			demonitor(MonitorRef, [flush]),
			State2;
		error ->
			State
	end.

is_alive(Pid) when is_pid(Pid) ->
	is_process_alive(Pid);
is_alive(Port) when is_port(Port) ->
	case erlang:port_info(Port, id) of
		{id, _} -> true;
		undefined -> false
	end;
is_alive(_) ->
	false.
