-module(lproc_bimaps).
% API
-export([
	new/0,
	add/3,
	size/1,
	find_by_key/2,
	find_by_value/2,
	take_by_key/2,
	take_by_value/2,
	has_key/2,
	has_value/2
]).
-export_type([bimap/2]).

-opaque bimap(K, V) :: {#{K => V}, #{V => K}}.

% API

-spec new() -> bimap(any(), any()).
new() -> {#{}, #{}}.

-spec size(bimap(dynamic(), dynamic())) -> non_neg_integer().
size({ByKey, _}) -> maps:size(ByKey).

-spec add(K, V, Map1) -> Map2 when
	Map1 :: bimap(K, V),
	Map2 :: bimap(K, V).
add(K, V, {ByKey, ByValue}) ->
	{
		ByKey#{K => V},
		ByValue#{V => K}
	}.

-spec find_by_key(K, bimap(K, V)) -> {ok, V} | error.
find_by_key(Key, {ByKey, _}) -> maps:find(Key, ByKey).

-spec find_by_value(V, bimap(K, V)) -> {ok, K} | error.
find_by_value(Value, {_, ByValue}) -> maps:find(Value, ByValue).

-spec take_by_key(K, Map1) -> {V, Map2} | error when
	Map1 :: bimap(K, V),
	Map2 :: bimap(K, V).
take_by_key(Key, {ByKey, ByValue}) ->
	case maps:take(Key, ByKey) of
		{Value, ByKey2} ->
			{Value, {ByKey2, maps:remove(Value, ByValue)}};
		error ->
			error
	end.

-spec take_by_value(V, Map1) -> {K, Map2} | error when
	Map1 :: bimap(K, V),
	Map2 :: bimap(K, V).
take_by_value(Value, {ByKey, ByValue}) ->
	case maps:take(Value, ByValue) of
		{Key, ByValue2} ->
			{Key, {maps:remove(Key, ByKey), ByValue2}};
		error ->
			error
	end.

-spec has_key(K, bimap(K, dynamic())) -> boolean().
has_key(Key, {ByKey, _}) ->
	maps:is_key(Key, ByKey).

-spec has_value(V, bimap(term(), V)) -> boolean().
has_value(Value, {_, ByValue}) ->
	maps:is_key(Value, ByValue).
