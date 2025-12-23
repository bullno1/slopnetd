#include <erl_nif.h>
#include <openssl/pkcs12.h>
#include <openssl/x509.h>
#include <openssl/evp.h>
#include <openssl/bio.h>
#include <openssl/err.h>

#define ENIF_CHECK(op) \
	do { \
		if (!(op)) { ret = enif_make_badarg(env); goto end; } \
	} while (0)

static ERL_NIF_TERM
make_pkcs12(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	ERL_NIF_TERM ret;

	X509* cert = NULL;
	EVP_PKEY* key = NULL;
	PKCS12* p12 = NULL;
	BIO* bio_p12 = NULL;

	ENIF_CHECK(argc == 2);

	ErlNifBinary der_cert;
	ErlNifBinary der_key;
	ENIF_CHECK(enif_inspect_binary(env, argv[0], &der_cert));
	ENIF_CHECK(enif_inspect_binary(env, argv[1], &der_key));

	BIO* bio_cert = BIO_new_mem_buf(der_cert.data, der_cert.size);
	cert = d2i_X509_bio(bio_cert, NULL);
	BIO_free(bio_cert);
	ENIF_CHECK(cert != NULL);

	BIO* bio_key = BIO_new_mem_buf(der_key.data, der_key.size);
	key = d2i_PrivateKey_bio(bio_key, NULL);
	BIO_free(bio_key);
	ENIF_CHECK(key != NULL);

	p12 = PKCS12_create("", "", key, cert, NULL, -1, -1, 0, 0, 0);

	bio_p12 = BIO_new(BIO_s_secmem());
	i2d_PKCS12_bio(bio_p12, p12);

	char *buf = NULL;
	long len = BIO_get_mem_data(bio_p12, &buf);
	ERL_NIF_TERM p12_bin;
	memcpy(enif_make_new_binary(env, len, &p12_bin), buf, len);

	ret = p12_bin;
end:
	if (cert) { X509_free(cert); }
	if (key) { EVP_PKEY_free(key); }
	if (p12) { PKCS12_free(p12); }
	if (bio_p12) { BIO_free(bio_p12); }

	return ret;
}

static ErlNifFunc nif_funcs[] = {
	{
		.name = "make_pkcs12", .arity = 2,
		.fptr = make_pkcs12,
		.flags = ERL_NIF_DIRTY_JOB_CPU_BOUND,
	},
};

ERL_NIF_INIT(snetd_crypto, nif_funcs, NULL, NULL, NULL, NULL)
