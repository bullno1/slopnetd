#include <erl_nif.h>

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wstrict-prototypes"
#endif
#include "cute_net.h"
#ifdef __clang__
#pragma clang diagnostic pop
#endif

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

static ErlNifFunc nif_funcs[] = {
	{
		.name = "nif_make_keypair", .arity = 0,
		.fptr = nif_make_keypair,
		.flags = ERL_NIF_DIRTY_JOB_CPU_BOUND,
	},
};

ERL_NIF_INIT(cn_crypto, nif_funcs, NULL, NULL, NULL, NULL)
