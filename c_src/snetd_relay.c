#include "slopnetd.h"

static void*
init(snetd_env_t* env) {
	return NULL;
}

static void
cleanup(void* ctx) {
}

static void
event(void* ctx, const snetd_event_t* event) {
}

static snetd_t snetd_relay = {
	.init = init,
	.cleanup = cleanup,
	.event = event,
};

SNETD_ENTRY(snetd_relay)
