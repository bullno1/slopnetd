-module(snetd_auth).
-export([issue_token/1, verify_token/1]).

-spec issue_token(binary()) -> binary().
issue_token(UserId) ->
	{ok, #{
		ttl := TokenTTL,
		keys := Keys,
		default_kid := Kid,
		signing_algorithm := SigningAlg
	}} = application:get_env(slopnetd, jwt),
	Claims = #{
		~"sub" => UserId,
		~"exp" => erlang:system_time(second) + TokenTTL
	},
	SigningOpts = #{
		key => maps:get(Kid, Keys),
		kid => Kid,
		algorithm => SigningAlg
	},
	jwt:issue(Claims,SigningOpts).

-spec verify_token(binary()) -> {ok, UserId :: binary()} | {error, term()}.
verify_token(Cookie) ->
	{ok, #{
		keys := Keys,
		verify_algorithms := Algorithms
	}} = application:get_env(slopnetd, jwt),
	VerifyOptions = #{
		keys => Keys,
		algorithms => Algorithms,
		validators => [
			jwt:validate_exp(),
			jwt:validate_sub()
		]
	},
	case jwt:decode(Cookie, VerifyOptions) of
		{ok, #{~"sub" := UserId}} -> {ok, UserId};
		{error, _} = Err -> Err
	end.
