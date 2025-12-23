-module(snetd_crypto_test).
-include_lib("eunit/include/eunit.hrl").

keymem_test_() ->
	{spawn,
	 [fun() ->
		{Cert, Key} = snetd_crypto:generate_cert([]),
		Options = #{
			alpn => ["h3"],
			certkeyasn1 => snetd_crypto:make_pkcs12(Cert, Key)
		},
		?assertMatch({ok, _Handle}, quicer:listen(8081, Options)),
		ok
	 end]
	}.

kefile_test_() ->
	Test = {setup,
	 fun() ->
		TmpBase = case os:getenv("TMPDIR") of
			false -> "/tmp";
			Dir -> Dir
		end,
		filename:join(TmpBase, "eunit_" ++ integer_to_list(erlang:unique_integer([monotonic, positive])))
	 end,
	 fun(Dir) -> file:del_dir_r(Dir) end,
	 fun(Dir) -> [fun() -> test_keyfile(Dir) end] end
	},
	{spawn, Test}.

test_keyfile(Dir) ->
	{Cert, Key} = snetd_crypto:generate_cert([]),
	CertFile = filename:join(Dir, "cert"),
	ok = filelib:ensure_dir(CertFile),
	ok = file:write_file(
		CertFile,
		public_key:pem_encode([{'Certificate', Cert, not_encrypted}])
	),
	KeyFile = filename:join(Dir, "key"),
	ok = file:write_file(
		KeyFile,
		public_key:pem_encode([{'ECPrivateKey', Key, not_encrypted}])
	),
	Options = #{
		alpn => ["h3"],
		certfile => CertFile,
		keyfile => KeyFile
	},
	?assertMatch({ok, _Handle}, quicer:listen(8081, Options)).
