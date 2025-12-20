#define _GNU_SOURCE
#include "sokol_time.h"
#include "slopnetd.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include <time.h>
#include <unistd.h>
#include <dlfcn.h>
#include <poll.h>

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wstrict-prototypes"
#endif
#include "cute_net.h"
#ifdef __clang__
#pragma clang diagnostic pop
#endif

typedef struct {
	char* ptr;
	int size;
} buf_t;

typedef struct {
	snetd_env_t env;
	cn_server_t* server;
	bool should_run;
} snetd_env_impl_t;

static void
ensure_buf(buf_t* buf, int size) {
	if (buf->size < size) {
		buf->ptr = realloc(buf->ptr, size);
		buf->size = size;
	}
}

static bool
read_exactly(char* buf, int len) {
	while (len > 0) {
		int bytes_read = read(3, buf, len);
		if (bytes_read <= 0) { return false; }

		buf += bytes_read;
		len -= bytes_read;
	}

	return true;
}

/*static bool*/
/*write_exactly(const char* buf, int len) {*/
	/*while (len > 0) {*/
		/*int bytes_written = write(4, buf, len);*/
		/*if (bytes_written <= 0) { return false; }*/

		/*buf += bytes_written;*/
		/*len -= bytes_written;*/
	/*}*/

	/*return true;*/
/*}*/

static int
recv_cmd(buf_t* buf) {
	char header[2];
	if (!read_exactly(header, sizeof(header))) { return 0; }

	int len = (header[0] << 8) | header[1];
	ensure_buf(buf, len);
	if (!read_exactly(buf->ptr, len)) { return 0; }

	return len;
}

static void*
snetd_env_realloc_impl(snetd_env_t* env, void* ptr, size_t size) {
	if (size == 0) {
		free(ptr);
		return NULL;
	} else {
		return realloc(ptr, size);
	}
}

static void
snetd_env_log_impl(snetd_env_t* env, const char* message) {
	fprintf(stderr, "%s\n", message);
}

static void
snetd_env_send_impl(snetd_env_t* env, int player, const void* data, size_t size, bool reliable) {
	snetd_env_impl_t* impl = (snetd_env_impl_t*)env;
	cn_server_send(impl->server, data, size, player, reliable);
}

static void
snetd_env_kick_impl(snetd_env_t* env, int player) {
	snetd_env_impl_t* impl = (snetd_env_impl_t*)env;
	cn_server_disconnect_client(impl->server, player, true);
}

static void
snetd_env_terminate_impl(snetd_env_t* env) {
	snetd_env_impl_t* impl = (snetd_env_impl_t*)env;
	impl->should_run = false;
}

static void
update_server(cn_server_t* server, uint64_t* timer, snetd_t* snetd, void* snetd_ctx) {
	cn_server_update(server, stm_sec(stm_laptime(timer)), time(NULL));

	cn_server_event_t event;
	while (cn_server_pop_event(server, &event)) {
		switch (event.type) {
			case CN_SERVER_EVENT_TYPE_NEW_CONNECTION:
				snetd->event(snetd_ctx, &(snetd_event_t){
					.type = SNETD_EVENT_PLAYER_JOINED,
					.player_joined = { .index = event.u.new_connection.client_index },
				});
				break;
			case CN_SERVER_EVENT_TYPE_DISCONNECTED:
				snetd->event(snetd_ctx, &(snetd_event_t){
					.type = SNETD_EVENT_PLAYER_LEFT,
					.player_left = { .index = event.u.disconnected.client_index },
				});
				break;
			case CN_SERVER_EVENT_TYPE_PAYLOAD_PACKET:
				snetd->event(snetd_ctx, &(snetd_event_t){
					.type = SNETD_EVENT_MESSAGE,
					.message = {
						.sender = event.u.payload_packet.client_index,
						.data = event.u.payload_packet.data,
						.size = event.u.payload_packet.size,
					},
				});
				cn_server_free_packet(server, event.u.payload_packet.client_index, event.u.payload_packet.data);
				break;
		}
	}
}

int
main(int argc, const char* argv[]) {
	stm_setup();

	void* module = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
	if (module == NULL) { return 1; }
	snetd_t* (*entry_fn)(void) = (snetd_t* (*)(void))dlsym(module, "snetd_entry");
	if (entry_fn == NULL) { dlclose(module); return 1; }
	snetd_t* snetd = entry_fn();

	// TODO: configure
	double broadcast_rate = 20;
	double tick_rate = 30;
	const double broadcast_interval_ms = 1000 / broadcast_rate;
	const double tick_interval_ms = 1000 / tick_rate;

	buf_t recv_buf = { 0 };
	int cmd_size;

	if ((cmd_size = recv_cmd(&recv_buf)) <= 0) {
		return 1;
	}

	if (cmd_size != (sizeof(cn_crypto_sign_public_t) + sizeof(cn_crypto_sign_secret_t))) {
		return 1;
	}

	cn_server_config_t config = cn_server_config_defaults();
	memcpy(&config.public_key, recv_buf.ptr, sizeof(config.public_key));
	memcpy(&config.secret_key, recv_buf.ptr + sizeof(config.public_key), sizeof(config.secret_key));
	cn_server_t* server = cn_server_create(config);
	cn_server_start(server, "0.0.0.0:0");

	snetd_env_impl_t env_impl = {
		.env = {
			.realloc = snetd_env_realloc_impl,
			.log = snetd_env_log_impl,
			.send = snetd_env_send_impl,
			.kick = snetd_env_kick_impl,
			.terminate = snetd_env_terminate_impl,
		},
		.server = server,
		.should_run = true,
	};
	void* snetd_ctx = snetd->init(&env_impl.env);

	uint64_t tick_timer = 0;
	uint64_t tick_count = 0;
	double tick_accumulator_ms = 0.0;

	uint64_t broadcast_timer = 0;
	double broadcast_accumulator_ms = 0.0;

	uint64_t server_update_timer = 0;

	uint64_t poll_timer = 0;
	double time_budget_ms = 0.0;

	while (env_impl.should_run) {
		stm_laptime(&poll_timer);
		time_budget_ms += tick_interval_ms;

		// Tick
		{
			tick_accumulator_ms += stm_ms(stm_laptime(&tick_timer));

			while (tick_accumulator_ms > tick_interval_ms) {
				snetd->event(snetd_ctx, &(snetd_event_t){
					.type = SNETD_EVENT_TICK,
					.tick = { .tick = tick_count },
				});
				++tick_count;
				tick_accumulator_ms -= tick_interval_ms;
			}
		}

		// Broadcast
		{
			broadcast_accumulator_ms += stm_ms(stm_laptime(&broadcast_timer));

			if (broadcast_accumulator_ms > broadcast_interval_ms) {
				snetd->event(snetd_ctx, &(snetd_event_t){
					.type = SNETD_EVENT_BROADCAST,
				});
				broadcast_accumulator_ms = fmod(broadcast_accumulator_ms, broadcast_interval_ms);
			}
		}

		// Server update
		update_server(server, &server_update_timer, snetd, snetd_ctx);

		// Use the rest of the timeslice for polling
		time_budget_ms -= stm_ms(stm_laptime(&broadcast_timer));
		do {
			int poll_result;
			int timeout = time_budget_ms > 0.0 ? (int)time_budget_ms : 0;
			do {
				bool injected_packets = false;
				struct pollfd polls = {
					.fd = 3,
					.events = POLLIN | POLLERR | POLLHUP
				};
				poll_result = poll(&polls, 1, timeout);
				timeout = 0;

				if ((polls.revents & POLLERR) | (polls.revents & POLLHUP)) {
					goto end;
				} else if (polls.revents & POLLIN) {
					// read command
					// injected_packets = true;
				}

				if (injected_packets) {  // Process injected packets
					update_server(server, &server_update_timer, snetd, snetd_ctx);
				}
			} while (poll_result > 0);

			time_budget_ms -= stm_ms(stm_laptime(&broadcast_timer));
		} while (time_budget_ms > 0.0);
	}
end:
	cn_server_stop(server);
	cn_server_destroy(server);

	snetd->cleanup(snetd_ctx);

	return 0;
}

#define SOKOL_IMPL
#include "sokol_time.h"
