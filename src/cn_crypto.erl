-module(cn_crypto).
-export([make_keypair/0, make_connect_token/1]).
-export_type([
	  token_options/0
	, public_key/0
	, secret_key/0
	, connect_token/0
]).
-on_load(load_nif/0).
-nifs([
	  nif_make_keypair/0
	, nif_make_connect_token/7
]).

-type token_options() :: #{
	application_id => non_neg_integer(),
	creation_timestamp => non_neg_integer(),
	expiration_timestamp => non_neg_integer(),
	handshake_timeout => non_neg_integer(),
	address_list => [{inet:ip_address(), inet:port_number()}],
	client_id => non_neg_integer(),
	secret_key := secret_key()
}.
-type public_key() :: <<_:32*8>>.
-type secret_key() :: <<_:64*8>>.
-type connect_token() :: <<_:1114*8>>.

%% Public

-spec make_keypair() -> {public_key(), secret_key()}.
make_keypair() ->
	nif_make_keypair().

-spec make_connect_token(token_options()) -> connect_token().
make_connect_token(#{ secret_key := SecretKey } = Opts) ->
	ApplicationId = maps:get(application_id, Opts, 0),
	CreationTimestamp = maps:get(creation_timestamp, Opts, 0),
	ExpirationTimestamp = maps:get(expiration_timestamp, Opts, 0),
	HandshakeTimeout = maps:get(handshake_timeout, Opts, 0),
	AddressList = maps:get(address_list, Opts, []),
	ClientId = maps:get(client_id, Opts, 0),
	nif_make_connect_token(
		ApplicationId,
		CreationTimestamp,
		ExpirationTimestamp,
		HandshakeTimeout,
		[format_endpoint(Endpoint) || Endpoint <- AddressList],
		ClientId,
		SecretKey
	).

%% Private

format_endpoint({IP, Port}) when tuple_size(IP) == 4 ->
    iolist_to_binary(io_lib:format("~s:~p\0", [inet:ntoa(IP), Port]));
format_endpoint({IP, Port}) when tuple_size(IP) == 8 ->
    iolist_to_binary(io_lib:format("[~s]:~p\0", [inet:ntoa(IP), Port]));
format_endpoint(_) ->
	error(badarg).

%% NIF

load_nif() ->
	erlang:load_nif(filename:join(code:priv_dir(slopnetd), "bin/cn_crypto"), 0).

nif_make_keypair() -> erlang:nif_error(not_loaded).

nif_make_connect_token(
	_ApplicationId,
	_CreationTimestamp,
	_ExpirationTimestamp,
	_HandshakeTimeout,
	_AddressList,
	_ClientId,
	_SecretKey
) ->
	erlang:nif_error(not_loaded).
