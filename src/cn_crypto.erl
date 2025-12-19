-module(cn_crypto).
-export([make_keypair/0]).
-on_load(load_nif/0).
-nifs([nif_make_keypair/0]).

%% Public

-spec make_keypair() -> {binary(), binary()}.
make_keypair() ->
	nif_make_keypair().

%% NIF

load_nif() ->
	erlang:load_nif(filename:join(code:priv_dir(slopnetd), "bin/cn_crypto"), 0).

nif_make_keypair() -> erlang:nif_error(not_loaded).
