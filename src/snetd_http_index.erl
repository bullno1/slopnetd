-module(snetd_http_index).
-behaviour(cowboy_handler).
-export([routes/0]).
-export([init/2]).

routes() ->
	[{"/", ?MODULE, []}].

init(Req, State) ->
    Req2 = cowboy_req:reply(200, #{
        ~"content-type" => ~"text/plain"
    }, ~"Hello World!", Req),
    {ok, Req2, State}.
