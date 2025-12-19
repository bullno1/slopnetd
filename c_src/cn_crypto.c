#include <erl_nif.h>

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wstrict-prototypes"
#endif
#include "cute_net.h"
#ifdef __clang__
#pragma clang diagnostic pop
#endif

#define ENIF_CHECK(op) \
	do { \
		if (!(op)) { return enif_make_badarg(env); } \
	} while (0)
#define MAX_NUM_ADDRESSES 32

static ERL_NIF_TERM
nif_make_keypair(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	cn_crypto_sign_public_t public;
	cn_crypto_sign_secret_t secret;
	cn_crypto_sign_keygen(&public, &secret);

	ERL_NIF_TERM nif_public, nif_secret;
	memcpy(enif_make_new_binary(env, sizeof(public.key), &nif_public), public.key, sizeof(public.key));
	memcpy(enif_make_new_binary(env, sizeof(secret.key), &nif_secret), secret.key, sizeof(secret.key));
	return enif_make_tuple(env, 2, nif_public, nif_secret);
}

static ERL_NIF_TERM
nif_make_connect_token(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	ENIF_CHECK(argc == 7);

	unsigned long application_id;
	ENIF_CHECK(enif_get_ulong(env, argv[0], &application_id));

	unsigned long creation_timestamp;
	ENIF_CHECK(enif_get_ulong(env, argv[1], &creation_timestamp));

	unsigned long expiration_timestamp;
	ENIF_CHECK(enif_get_ulong(env, argv[2], &expiration_timestamp));

	unsigned long handshake_timeout;
	ENIF_CHECK(enif_get_ulong(env, argv[3], &handshake_timeout));

	int address_count = 0;
	const char* address_list[MAX_NUM_ADDRESSES];
	ERL_NIF_TERM itr = argv[4];
	while (true) {
		if (enif_is_empty_list(env, itr)) { break; }

		ENIF_CHECK(address_count < MAX_NUM_ADDRESSES);
		ERL_NIF_TERM head, tail;
		ENIF_CHECK(enif_get_list_cell(env, itr, &head, &tail));

		ErlNifBinary address;
		ENIF_CHECK(enif_inspect_binary(env, head, &address));
		address_list[address_count++] = (const char*)address.data;

		itr = tail;
	}
	ENIF_CHECK(1 <= address_count && address_count <= 32);

	unsigned long client_id;
	ENIF_CHECK(enif_get_ulong(env, argv[5], &client_id));

	ErlNifBinary secret_key;
	ENIF_CHECK(enif_inspect_binary(env, argv[6], &secret_key));
	ENIF_CHECK(secret_key.size == sizeof(cn_crypto_sign_secret_t));

	cn_crypto_key_t client_to_server_key = cn_crypto_generate_key();
	cn_crypto_key_t server_to_client_key = cn_crypto_generate_key();
	ERL_NIF_TERM connect_token;
	cn_result_t result = cn_generate_connect_token(
		application_id,
		creation_timestamp,
		&client_to_server_key,
		&server_to_client_key,
		expiration_timestamp,
		handshake_timeout,
		address_count,
		address_list,
		client_id,
		NULL,
		(cn_crypto_sign_secret_t*)secret_key.data,
		enif_make_new_binary(env, CN_CONNECT_TOKEN_SIZE, &connect_token)
	);
	ENIF_CHECK(!cn_is_error(result));

	return connect_token;
}

static ErlNifFunc nif_funcs[] = {
	{
		.name = "nif_make_keypair", .arity = 0,
		.fptr = nif_make_keypair,
		.flags = ERL_NIF_DIRTY_JOB_CPU_BOUND,
	},
	{
		.name = "nif_make_connect_token", .arity = 7,
		.fptr = nif_make_connect_token,
		.flags = ERL_NIF_DIRTY_JOB_CPU_BOUND,
	},
};

ERL_NIF_INIT(cn_crypto, nif_funcs, NULL, NULL, NULL, NULL)
