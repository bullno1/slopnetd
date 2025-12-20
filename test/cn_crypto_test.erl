-module(cn_crypto_test).
-include_lib("eunit/include/eunit.hrl").

make_keypair_test() ->
	{Public, Private} = cn_crypto:make_keypair(),
	?assert(is_binary(Public)),
	?assert(is_binary(Private)).

make_connect_token_test() ->
	{_Public, Private} = cn_crypto:make_keypair(),
	Token = cn_crypto:make_connect_token(#{
		application_id => 0,
		creation_timestamp => erlang:system_time(second),
		expiration_timestamp => erlang:system_time(second) + 10,
		handshake_timeout => 5,
		address_list => [{{127, 0, 0, 1}, 8080}, {{0,0,0,0,0,0,0,1}, 8080}],
		client_id => 0,
		secret_key => Private
	}),
	?assert(is_binary(Token)),
	?assert(byte_size(Token) =:= 1114).
