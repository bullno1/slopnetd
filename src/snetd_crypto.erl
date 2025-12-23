-module(snetd_crypto).
-export([generate_cert/1, make_pkcs12/2]).
-on_load(load_nif/0).
-nifs([make_pkcs12/2]).
-include_lib("public_key/include/public_key.hrl").

%% Public

-spec make_pkcs12(public_key:der_encoded(), public_key:der_encoded()) -> binary().
make_pkcs12(_Cert, _Key) ->
       erlang:nif_error(not_loaded).

-spec generate_cert(IpAddresses) -> { Cert, Key } when
	IpAddresses :: inet:ip_address(),
	Cert :: public_key:der_encoded(),
	Key :: public_key:der_encoded().
generate_cert(_IpAddresses) ->
	From = os:timestamp(),
	To = add_seconds(From, 7 * 86400),
	Key = public_key:generate_key({namedCurve, ?secp256r1}),
	Subject = {rdnSequence, [[#'AttributeTypeAndValue'{type = ?'id-at-commonName', value = {utf8String, ~"localhost"}}]]},
	Validity = #'Validity'{
		notBefore = {utcTime, timestamp_to_utc(From)},
		notAfter = {utcTime, timestamp_to_utc(To)}
	},
	PubKeyInfo = #'OTPSubjectPublicKeyInfo'{
		algorithm = #'PublicKeyAlgorithm'{
			algorithm = ?'id-ecPublicKey',
			parameters = {namedCurve, ?secp256r1}
		},
		subjectPublicKey = #'ECPoint'{ point = Key#'ECPrivateKey'.publicKey }
	},
	Extensions = [
		#'Extension'{
			extnID = ?'id-ce-subjectAltName',
			critical = false,
			extnValue = [{dNSName, "localhost"}, {iPAddress, <<127,0,0,1>>}]
		},
		#'Extension'{
			extnID = ?'id-ce-basicConstraints',
			critical = true,
			extnValue = #'BasicConstraints'{cA = false}
		},
		#'Extension'{
			extnID = ?'id-ce-keyUsage',
			critical = true,
			extnValue = [digitalSignature]
		},
		#'Extension'{
			extnID = ?'id-ce-extKeyUsage',
			critical = false,
			extnValue = [?'id-kp-serverAuth']
		}
	],
	TBS = #'OTPTBSCertificate'{
		version = v3,
		serialNumber = erlang:unique_integer([positive, monotonic]),
		signature = #'SignatureAlgorithm'{algorithm = ?'ecdsa-with-SHA256'},
		issuer = Subject,
		validity = Validity,
		subject = Subject,
		subjectPublicKeyInfo = PubKeyInfo,
		extensions = Extensions
	},
    % Sign to get DER-encoded cert
    CertDer = public_key:pkix_sign(TBS, Key),
    KeyDer = public_key:der_encode('ECPrivateKey', Key),
    {CertDer, KeyDer}.

%% Private

add_seconds({Mega, Sec, Micro}, AddSecs) ->
    TotalSec = Mega * 1000000 + Sec + AddSecs,
    NewMega = TotalSec div 1000000,
    NewSec = TotalSec rem 1000000,
    {NewMega, NewSec, Micro}.

timestamp_to_utc({Mega, Sec, _Micro}) ->
    {{Y, M, D}, {H, Min, S}} = calendar:now_to_universal_time({Mega, Sec, 0}),
    YY = Y rem 100,
    lists:flatten(io_lib:format("~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0BZ", [YY, M, D, H, Min, S])).

%% NIF

load_nif() ->
       erlang:load_nif(filename:join(code:priv_dir(slopnetd), "bin/snetd_crypto"), 0).
