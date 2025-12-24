-module(snetd_h3).
-behaviour(snetd_quic_connection).
-export([generate_quic_cert/1, rotate_cert/2, child_spec/0, start_link/0]).
-export([init/3]).

%% Public

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
	#{ id => ?MODULE
	 , start => {?MODULE, start_link, []}
	 , type => supervisor
	 , modules => [snetd_quic]
	 }.

start_link() ->
	ProtoOpts = #{
		env => #{dispatch => {persistent_term, snetd_http}}
	},
	#{ preferred_key := {_, {CertDer, KeyDer}}} = keymaker:info({slopnetd, quic}),
	{ok, QuicOpts} = application:get_env(slopnetd, quic),
	TransportOpts = inject_cert(QuicOpts, CertDer, KeyDer),
	snetd_quic:start_link({local, ?MODULE}, ?MODULE, ProtoOpts, TransportOpts).

generate_quic_cert(_) ->
	{ok, Addresses} = inet:getifaddrs(),
	Addrs = [proplists:get_value(addr, Props) || {_Interface, Props} <- Addresses],
	snetd_crypto:generate_cert(Addrs).

rotate_cert({CertDer, KeyDer}, _) ->
	{ok, QuicOpts} = application:get_env(slopnetd, quic),
	TransportOpts = inject_cert(QuicOpts, CertDer, KeyDer),
	snetd_quic:reload(?MODULE, TransportOpts).

%% snetd_quic_connection

init(Parent, Args, Conn) ->
	cowboy_http3:init(Parent, make_ref(), Conn, Args).

%% Private

-spec inject_cert(snetd_quic:opts(), binary(), binary()) -> snetd_quic:opts().
inject_cert(QuicOpts, CertDer, KeyDer) ->
	DefaultListenOpts = #{
		alpn => ["h3"],
		peer_unidi_stream_count => 5,
		peer_bidi_stream_count => 5,
		datagram_send_enabled => 1,
		datagram_receive_enabled => 1,
		certkeyasn1 => snetd_crypto:make_pkcs12(CertDer, KeyDer)
	},
	ListenOpts = maps:get(listen_opts, QuicOpts, #{}),
	QuicOpts#{ listen_opts => maps:merge(DefaultListenOpts, ListenOpts) }.
