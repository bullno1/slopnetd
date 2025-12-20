-module(snetd_auth).
-export([issue_token/1, verify_token/1, auth_req/1]).
-export_type([user/0]).

-type user() :: #{ id := binary() }.

-spec issue_token(user()) -> binary().
issue_token(#{ id := UserId }) ->
	{ok, #{
		ttl := TokenTTL,
		signing_algorithm := SigningAlg
	}} = application:get_env(slopnetd, jwt),
	#{ preferred_key := {Kid, Key} } = keymaker:info({slopnetd, jwt}),
	Claims = #{
		~"sub" => UserId,
		~"exp" => erlang:system_time(second) + TokenTTL
	},
	SigningOpts = #{
		kid => Kid,
		key => Key,
		algorithm => SigningAlg
	},
	jwt:issue(Claims, SigningOpts).

-spec verify_token(binary()) -> {ok, user()} | {error, term()}.
verify_token(Cookie) ->
	{ok, #{
		verify_algorithms := Algorithms
	}} = application:get_env(slopnetd, jwt),
	#{ keys := Keys } = keymaker:info({slopnetd, jwt}),
	VerifyOptions = #{
		keys => Keys,
		algorithms => Algorithms,
		validators => [
			jwt:validate_exp(),
			jwt:validate_sub()
		]
	},
	case jwt:decode(Cookie, VerifyOptions) of
		{ok, #{~"sub" := UserId}} -> {ok, #{ id => UserId }};
		{error, _} = Err -> Err
	end.

-spec auth_req(cowboy_req:req()) -> {ok, user()} | {error, {unauthorized, term()}}.
auth_req(Req) ->
	case cowboy_req:parse_header(~"authorization", Req) of
		{bearer, Token} ->
			case verify_token(Token) of
				{ok, _} = Authorized -> Authorized;
				{error, Reason} -> {error, {unauthorized, Reason}}
			end;
		_ -> {error, {unauthorized, missing_header}}
	end.
