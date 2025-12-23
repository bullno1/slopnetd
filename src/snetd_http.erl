-module(snetd_http).
-export([start/0, stop/0]).
-export([on_reload/0]).
-on_reload(on_reload/0).

%% Public

start() ->
	load_routes(),
    {ok, _} = cowboy:start_clear(?MODULE,
        [{port, 8080}],
        #{env => #{dispatch => {persistent_term, ?MODULE}}}
    ),
	ok.

stop() ->
	cowboy:stop_listener(?MODULE).

on_reload() ->
	load_routes().

%% Private

load_routes() ->
	Dispatch = cowboy_router:compile([
		{'_', routes()}
	]),
	persistent_term:put(?MODULE, Dispatch).

routes() ->
	snetd_http_index:routes()
	++ snetd_auth_itchio:routes()
	++ snetd_auth_cookie:routes()
	++ snetd_lobby:routes()
	++ [
		{"/assets/[...]", cowboy_static, {priv_dir, slopnetd, "www/assets", [
			{mimetypes, cow_mimetypes, all}
		]}}
	].
