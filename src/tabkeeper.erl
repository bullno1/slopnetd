-module(tabkeeper).
-behaviour(gen_server).

%% API.
-export([
	start_link/0,
	new/2,
	recover/1,
	drop/1
]).

%% gen_server.
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec new(atom(), list()) -> ets:tid().
new(Name, Options) ->
	case recover(Name) of
		{ok, Tid} -> Tid;
		error ->
			HeirOpt = {heir, whereis(?MODULE), []},
			ets:new(Name, lists:keystore(heir, 1, Options, HeirOpt))
	end.

-spec recover(atom()) -> {ok, ets:tid()} | error.
recover(Name) ->
	Ref = make_ref(),
	case gen_server:call(?MODULE, {recover, Name, Ref}) of
		ok -> receive {'ETS-TRANSFER', Tid, _, Ref} -> {ok, Tid} end;
		error -> error
	end.

-spec drop(atom()) -> ok.
drop(Name) -> gen_server:call(?MODULE, {drop, Name}).

%% gen_server.

init([]) -> {ok, []}.

handle_call({recover, Name, Ref}, {Pid, _}, State) ->
	try ets:give_away(Name, Pid, Ref) of
		_ -> {reply, ok, State}
	catch
		_:_ -> {reply, error, State}
	end;
handle_call({drop, Name}, _From, State) ->
	catch ets:delete(Name),
	{reply, ok, State};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
