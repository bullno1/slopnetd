-module(snetd_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	snetd_http:start(),
	inets:start(httpc, [{profile, snetd_itchio}]),
	snetd_sup:start_link().

stop(_State) ->
	inets:stop(httpc, snetd_itchio),
	snetd_http:stop(),
	ok.

%% internal functions
