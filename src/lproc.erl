-module(lproc).
% API
-export([
	register_name/2,
	unregister_name/1,
	whereis_name/1,
	send/2,
	subscribe/1,
	subscribe/2,
	unsubscribe/1,
	publish/2,
	list/1
]).

% API

-spec register_name(term(), pid() | port()) -> yes | no.
register_name(Name, PidOrPort) when is_pid(PidOrPort); is_port(PidOrPort) ->
	gen_server:call(lproc_name, {register_name, Name, PidOrPort}, infinity);
register_name(Name, PidOrPort) ->
	error(badarg, [Name, PidOrPort]).

-spec unregister_name(term()) -> ok.
unregister_name(Name) ->
	gen_server:call(lproc_name, {unregister_name, Name}, infinity).

-spec whereis_name(term()) -> pid() | port() | undefined.
whereis_name(Name) ->
	try ets:lookup_element(lproc_name, Name, 2) of
		Pid when is_pid(Pid) ->
			case is_process_alive(Pid) of
				true -> Pid;
				false -> undefined
			end;
		Port when is_port(Port) ->
			Port
	catch
		error:badarg -> undefined
	end.

-spec send(term(), term()) -> pid() | port().
send(Name, Msg) ->
	case whereis_name(Name) of
		Pid when is_pid(Pid) ->
			Pid ! Msg,
			Pid;
		Port when is_port(Port) ->
			_ = catch port_command(Port, Msg),
			Port;
		undefined ->
			exit({badarg, {Name, Msg}})
	end.

-spec subscribe(term()) -> ok.
subscribe(Topic) -> subscribe(Topic, []).

-spec subscribe(term(), term()) -> ok.
subscribe(Topic, Extra) ->
	gen_server:call(lproc_pubsub, {subscribe, Topic, Extra}, infinity).

-spec unsubscribe(term()) -> ok.
unsubscribe(Topic) ->
	gen_server:call(lproc_pubsub, {unsubscribe, Topic}, infinity).

-spec publish(term(), term()) -> ok.
publish(Topic, Msg) ->
	_ = [Proc ! Msg || [Proc] <- ets:match(lproc_pubsub, {{Topic, '$1'}, '_'})],
	ok.

-spec list(term()) -> [{pid(), term()}].
list(Topic) ->
	ets:select(lproc_pubsub, [
		{{{Topic, '$1'}, '$2'}, [], [{{'$1','$2'}}]}
	]).
