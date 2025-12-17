-module(snet_http_index).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req, State) ->
    Req = cowboy_req:reply(200, #{
        ~"content-type" => ~"text/plain"
    }, ~"Hello World!", Req),
    {ok, Req, State}.
