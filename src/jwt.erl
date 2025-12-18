-module(jwt).
-export([issue/2, decode/2]).
-export([sign_hs256/2, verify_hs256/3]).
-export([validate_exp/0, validate_exp/1]).
-export([validate_sub/0]).
-export_type([issue_options/0]).

-type issue_options() :: #{
	key := binary(),
	kid => binary(),
	algorithm := { Name :: binary(), Fun :: signing_fun() }
}.
-type signing_fun() :: fun((Content :: binary(), Key :: binary()) -> binary()).

-type decode_options() :: #{
	keys := #{ Kid :: binary() => Key :: binary() },
	algorithms := #{ Name :: binary() => Fun :: sig_verify_fun() },
	validators => [{Claim :: binary(), Fun :: claim_validate_fun()}]
}.
-type sig_verify_fun() :: fun((Content :: binary(), Signature :: binary(), Key :: binary()) -> boolean()).
-type claim_validate_fun() :: fun((Value :: term()) -> boolean()).

-spec issue(map(), issue_options()) -> binary().
issue(Claims, #{key := Key, algorithm := {Alg, SigningFun}, kid := Kid}) ->
	Header = #{
		~"alg" => Alg,
		~"kid" => Kid,
		~"typ" => ~"JWT"
	},
	HeaderBin = b64_encode(iolist_to_binary(json:encode(Header))),
	ClaimsBin = b64_encode(iolist_to_binary(json:encode(Claims))),
	HeaderAndClaims = <<HeaderBin/binary, $., ClaimsBin/binary>>,
	Signature = b64_encode(SigningFun(HeaderAndClaims, Key)),
	<<HeaderAndClaims/binary, $., Signature/binary>>.

-spec decode(binary(), decode_options()) -> {ok, map()} | {error, term()}.
decode(Token, #{ keys := Keys, algorithms := Algorithms } = Options) ->
	maybe
		{ok, [HeaderB64, ClaimsB64, SignatureB64]} ?=
			case binary:split(Token, <<$.>>, [global]) of
				[_, _, _] = Parts ->
					{ok, Parts};
				_ ->
					{error, invalid_token}
			end,
		{ok, HeaderBin} ?= b64_decode(HeaderB64),
		{ok, Header} ?= json_decode(HeaderBin),
		{ok, Kid, Alg} ?= case Header of
			#{ ~"kid" := Kid_, ~"alg" := Alg_ } ->
				{ok, Kid_, Alg_};
			_ ->
				{error, invalid_header}
		end,
		{ok, Key} ?= case maps:find(Kid, Keys) of
			{ok, _} = KeyResult -> KeyResult;
			error -> {error, invalid_kid}
		end,
		{ok, VerifYFun} ?= case maps:find(Alg, Algorithms) of
			{ok, _} = AlgResult -> AlgResult;
			error -> {error, invalid_alg}
		end,
		{ok, ClaimsBin} ?= b64_decode(ClaimsB64),
		{ok, SignatureBin} ?= b64_decode(SignatureB64),
		ok ?= case VerifYFun(<<HeaderB64/binary, $., ClaimsB64/binary>>, SignatureBin, Key) of
			true -> ok;
			false -> {error, invalid_sig}
		end,
		{ok, Claims} ?= json_decode(ClaimsBin),
		Validators = maps:get(validators, Options, [
			validate_exp(),
			validate_sub()
		]),
		ok ?= validate_claims(Claims, Validators),
		{ok, Claims}
	end.

-spec verify_hs256(binary(), binary(), binary()) -> boolean().
verify_hs256(Content, Signature, Key) ->
	crypto:hash_equals(sign_hs256(Content, Key), Signature).

-spec sign_hs256(binary(), binary()) -> binary().
sign_hs256(Content, Key) ->
	crypto:mac(hmac, sha3_256, Key, Content).

-spec validate_exp() -> { binary(), claim_validate_fun() }.
validate_exp() -> validate_exp(erlang:system_time(second)).

-spec validate_exp(integer()) -> { binary(), claim_validate_fun() }.
validate_exp(CurrentTime) ->
	VerifyFun = fun (Exp) -> is_integer(Exp) andalso CurrentTime < Exp end,
	{ ~"exp", VerifyFun }.

-spec validate_sub() -> { binary(), claim_validate_fun() }.
validate_sub() ->
	{ ~"sub", fun(Val) -> is_binary(Val) end }.

%% Private

b64_encode(Data) ->
	base64:encode(Data, #{ padding => false, mode => urlsafe }).

b64_decode(Data) ->
	try base64:decode(Data, #{ padding => false, mode => urlsafe }) of
		Result -> {ok, Result}
	catch
		error:Reason ->
			{error, {invalid_base64, Reason}}
	end.

json_decode(Str) ->
	try json:decode(Str) of
		Result -> {ok, Result}
	catch
		error:Reason ->
			{error, {invalid_json, Reason}}
	end.

validate_claims(_Claims, []) ->
	ok;
validate_claims(Claims, [{Name, Validator} | Rest]) ->
	case maps:find(Name, Claims) of
		{ok, Value} ->
			case Validator(Value) of
				true -> validate_claims(Claims, Rest);
				false -> {error, {invalid_claim, Name}}
			end;
		error ->
			{error, {invalid_claim, Name}}
	end.
