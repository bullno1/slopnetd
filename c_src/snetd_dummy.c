#include "slopnetd.h"

typedef struct {
	snetd_env_t* env;
} ctx_t;

static void*
init(snetd_env_t* env, const snetd_game_options_t* options) {
	ctx_t* ctx = snetd_realloc(env, NULL, sizeof(ctx_t));
	*ctx = (ctx_t){
		.env = env,
	};
	return ctx;
}

static void
cleanup(void* userdata) {
	ctx_t* ctx = userdata;
	snetd_realloc(ctx->env, ctx, 0);
}

static void
event(void* userdata, const snetd_event_t* event) {
	ctx_t* ctx = userdata;
	snetd_env_t* env = ctx->env;

	if (event->type == SNETD_EVENT_PLAYER_JOINING) {
		snetd_allow_join(env);
	}
}

static snetd_t snetd = {
	.init = init,
	.cleanup = cleanup,
	.event = event,
};

SNETD_ENTRY(snetd)
