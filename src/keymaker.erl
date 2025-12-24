-module(keymaker).
-behaviour(gen_server).
-export([
	start_link/1,
	info/1,
	new_key/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([options/0, store_options/0, key_gen_fun/0, store_info/0]).
-include_lib("kernel/include/logger.hrl").

-type options() :: #{
	path := file:name(),
	stores := #{ Name :: term() => store_options() }
}.
-type store_options() :: #{
	ttl_s := non_neg_integer(),
	generator := { key_gen_fun(), Options :: term() },
	on_rotate => { rotate_fun(), Options :: term() }
}.
-type key_gen_fun() :: fun((Options :: term()) -> term()).
-type rotate_fun() :: fun((Key :: term(), Options :: term()) -> ok).

-type store_info() :: #{
	preferred_key := {key_id(), term()},
	keys := #{ KeyId :: key_id() => Info :: term() }
}.
-type key_id() :: binary().

-record(state, {
	table :: dets:tab_name(),
	stores :: #{ Name :: term() => store_options() }
}).

%% Public

start_link(Options) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Options, [{hibernate_after, 5000}]).

-spec info(term()) -> store_info().
info(Store) ->
	Keys = dets:select(?MODULE, [{{{Store,'$1'},'$2'},[],[{{'$1','$2'}}]}]),
	{KeyId, Key} = lists:foldl(fun max/2, {0, []}, Keys),
	#{ preferred_key => { integer_to_binary(KeyId), Key}
	 , keys => #{ integer_to_binary(Kid) => Data || {Kid, Data} <- Keys}
	 }.

-spec new_key(term()) -> ok.
new_key(Store) ->
	gen_server:call(?MODULE, {new_key, Store}).

%% gen_server

init(#{ path := Path, stores := Stores }) ->
	{ok, Tab} = dets:open_file(?MODULE, [
		{file, Path},
		{ram_file, true},
		{repair, true}
	]),
	State = #state{
		table = Tab,
		stores = Stores
	},
	{ok, refresh_all(State)}.

handle_call({new_key, StoreName}, _From, #state{ table = Tab, stores = Stores } = State) ->
	case maps:find(StoreName, Stores) of
		{ok, #{ generator := { KeyGenFun, KeyGenOpts } } = Store} ->
			?LOG_INFO("Generating a new key for store ~p", [StoreName]),
			CurrentTime = erlang:system_time(second),
			Key = KeyGenFun(KeyGenOpts),
			dets:insert(Tab, {{StoreName, CurrentTime}, Key}),
			case Store of
				#{on_rotate := {OnRotate, RotateOpts}} ->
					?LOG_INFO("Rotating key for store ~p", [StoreName]),
					_ = catch OnRotate(Key, RotateOpts);
				_ ->
					ok
			end,
			dets:sync(Tab);
		error ->
			ok
	end,
	{reply, ok, State};
handle_call(Req, _From, State) ->
	?LOG_WARNING(#{ what => unknown_call, request => Req }),
	{noreply, State}.

handle_cast(_Req, State) ->
	{noreply, State}.

handle_info({refresh, StoreName}, State) ->
	{noreply, refresh_store(StoreName, State)};
handle_info(_, State) ->
	{noreply, State}.

%% Private

refresh_all(#state{ stores = Stores } = State) ->
	lists:foldl(
		fun refresh_store/2,
		State,
		maps:keys(Stores)
	).

refresh_store(StoreName, #state{ table = Tab, stores = Stores } = State) ->
	case maps:find(StoreName, Stores) of
		{ok, #{ ttl_s := TTL, generator := { KeyGenFun, KeyGenOpts } } = Store} ->
			CurrentTime = erlang:system_time(second),

			?LOG_INFO("Cleaning up store ~p", [StoreName]),
			ExpireTime = CurrentTime - TTL,
			_ = dets:select_delete(Tab, [{{{StoreName,'$1'},'_'},[{'=<','$1',ExpireTime}],[true]}]),

			LatestKeyTimetamp = lists:foldl(
				fun max/2,
				0,
				dets:select(Tab, [{{{StoreName,'$1'},'_'},[],['$1']}])
			),
			LatestKeyTTL = LatestKeyTimetamp - ExpireTime,
			case LatestKeyTTL =< (TTL div 2) of
				true ->
					?LOG_INFO("Generating a new key for store ~p", [StoreName]),
					Key = KeyGenFun(KeyGenOpts),
					dets:insert(Tab, {{StoreName, CurrentTime}, Key}),
					case Store of
						#{on_rotate := {OnRotate, RotateOpts}} ->
							?LOG_INFO("Rotating key for store ~p", [StoreName]),
							_ = catch OnRotate(Key, RotateOpts);
						_ ->
							ok
					end;
				false ->
					ok
			end,
			dets:sync(Tab),
			erlang:send_after((TTL div 2) * 1000, self(), {refresh, StoreName}),
			ok;
		error ->
			ok
	end,
	State.
