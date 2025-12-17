-module(snetd_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	snet_http:start(),
	snetd_sup:start_link().

stop(_State) ->
	snet_http:stop(),
	ok.

%% internal functions
