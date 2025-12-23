-module(snetd_crypto_test).
-include_lib("eunit/include/eunit.hrl").

keymem_test() ->
	{Cert, Key} = snetd_crypto:generate_cert([]),
	Options = #{
		alpn => ["h3"],
		certkeyasn1 => snetd_crypto:make_pkcs12(Cert, Key)
	},
	?assertMatch({ok, _Handle}, quicer:listen(8081, Options)),
	ok.
