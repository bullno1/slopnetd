#include "sokol_time.h"
#include "slopnetd.h"
#include "barg.h"
#include "ckit.h"
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

#define SNETD_MAX_NUM_PLAYERS 32
#define SNETD_MAX_NUM_ADDRESSES 16
#define SNETD_USER_ALREADY_CONNECTED -2
#define SNETD_SERVER_FULL -1

typedef char endpoint_str_t[sizeof("256.256.256.256:65536")];

typedef struct {
	char* ptr;
	int size;
} buf_t;

typedef enum {
	TRANSPORT_TYPE_CUTE_NET,
	TRANSPORT_TYPE_WEBTRANSPORT,
} transport_type_t;

typedef struct {
	const char* username;
	transport_type_t tranport_type;
	int transport_handle;
} player_info_t;

typedef struct {
	snetd_env_t env;
	cn_server_t* server;
	bool should_run;

	bool allow_join;
	const char* rejection_reason;

	player_info_t players[SNETD_MAX_NUM_PLAYERS];
	int cn_to_snet[CN_SERVER_MAX_CLIENTS];
	int num_players;
} snetd_env_impl_t;

typedef enum {
	PORT_CMD_REQUEST_CONNECT_TOKEN,
	PORT_CMD_REQUEST_JOIN_PERMISSION,
	PORT_CMD_WEBTRANSPORT_CONNECT,
	PORT_CMD_WEBTRANSPORT_DISCONNECT,
	PORT_CMD_WEBTRANSPORT_MESSAGE,
} request_message_type_t;

typedef enum {
	PORT_MSG_LOG_MESSAGE,
	PORT_MSG_QUERY_RESPONSE,
	PORT_MSG_WEBTRANSPORT_SEND,
	PORT_MSG_WEBTRANSPORT_DISCONNECT,
} control_message_type_t;

typedef struct {
	uint16_t port;
} init_done_t;

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

static bool
write_exactly(const char* buf, int len) {
	while (len > 0) {
		int bytes_written = write(4, buf, len);
		if (bytes_written <= 0) { return false; }

		buf += bytes_written;
		len -= bytes_written;
	}

	return true;
}

static int
recv_cmd(buf_t* buf) {
	char header[2];
	if (!read_exactly(header, sizeof(header))) { return 0; }

	int len = (header[0] << 8) | header[1];
	ensure_buf(buf, len);
	if (!read_exactly(buf->ptr, len)) { return 0; }

	return len;
}

static bool
send_control_header(control_message_type_t type, int payload_size) {
	uint16_t size = payload_size + 1;
	char hdr[3] = { (uint8_t)(size >> 8), (uint8_t)(size & 0xff), (uint8_t)type };
	return write_exactly(hdr, sizeof(hdr));
}

static inline bool
send_payload(const void* payload, int payload_size) {
	return write_exactly(payload, payload_size);
}

// The cn_server wrapper and the snetd_game are always updated together so we can just
// send a struct
static bool
send_control_message(control_message_type_t type, const void* payload, int payload_size) {
	if (!send_control_header(type, payload_size)) { return false; }
	if (!send_payload(payload, payload_size)) { return false; }

	return true;
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
	send_control_message(PORT_MSG_LOG_MESSAGE, message, message != NULL ? strlen(message) : 0);
}

static void
snetd_env_send_impl(snetd_env_t* env, int player, const void* data, size_t size, bool reliable) {
	snetd_env_impl_t* impl = (snetd_env_impl_t*)env;

	if (player >= SNETD_MAX_NUM_PLAYERS) { return; }
	const player_info_t* player_info = &impl->players[player];
	if (player_info->username == NULL) { return; }

	if (player_info->tranport_type == TRANSPORT_TYPE_CUTE_NET) {
		cn_server_send(impl->server, data, size, player_info->transport_handle, reliable);
	} else if (player_info->tranport_type == TRANSPORT_TYPE_WEBTRANSPORT) {
		send_control_header(PORT_MSG_WEBTRANSPORT_SEND, size + 2);
		char hdr[2] = { player_info->transport_handle, reliable };
		send_payload(hdr, sizeof(hdr));
		send_payload(data, size);
	}
}

static void
snetd_env_kick_impl(snetd_env_t* env, int player) {
	snetd_env_impl_t* impl = (snetd_env_impl_t*)env;

	if (player >= SNETD_MAX_NUM_PLAYERS) { return; }
	const player_info_t* player_info = &impl->players[player];
	if (player_info->username == NULL) { return; }

	if (player_info->tranport_type == TRANSPORT_TYPE_CUTE_NET) {
		cn_server_disconnect_client(impl->server, player_info->transport_handle, true);
	} else if (player_info->tranport_type == TRANSPORT_TYPE_WEBTRANSPORT) {
		char msg = player_info->transport_handle;
		send_control_message(PORT_MSG_WEBTRANSPORT_DISCONNECT, &msg, sizeof(msg));
	}
}

static void
snetd_env_terminate_impl(snetd_env_t* env) {
	snetd_env_impl_t* impl = (snetd_env_impl_t*)env;
	impl->should_run = false;
}

static void
snetd_env_allow_join_impl(snetd_env_t* env) {
	snetd_env_impl_t* impl = (snetd_env_impl_t*)env;
	impl->allow_join = true;
}

static void
snetd_env_forbid_join_impl(snetd_env_t* env, const char* reason) {
	snetd_env_impl_t* impl = (snetd_env_impl_t*)env;
	impl->allow_join = false;
	impl->rejection_reason = reason;
}

static int
alloc_player_slot(snetd_env_impl_t* env, const char* username) {
	int i, free_slot;
	free_slot = SNETD_SERVER_FULL;
	for (i = 0; i < SNETD_MAX_NUM_PLAYERS; ++i) {
		if (env->players[i].username == username) {  // username is interned
			// User is already connected
			return SNETD_USER_ALREADY_CONNECTED;
		}

		if (env->players[i].username == NULL && free_slot < 0) {
			free_slot = i;
			// Always scan all slots for duplicates before allocation
		}
	}

	if (free_slot >= 0) {
		++(env->num_players);
	}

	return free_slot;
}

int
main(int argc, const char* argv[]) {
	snetd_game_options_t options = { 0 };
	int port = 0;
	int connect_timeout_s = 10;
	int int_broadcast_rate = 20;
	int int_tick_rate = 30;

	const char* addresses[SNETD_MAX_NUM_ADDRESSES] = { "127.0.0.1" };
	barg_array_opts_t addresses_barg = {
		.element_parser = barg_str(&addresses[0]),
		.element_size = sizeof(addresses[0]),
		.max_num_elements = sizeof(addresses) / sizeof(addresses[0]),
	};

	barg_opt_t opts[] = {
		{
			.name = "created-by",
			.value_name = "username",
			.parser = barg_str(&options.created_by),
			.summary = "Name of the user who created the game",
		},
		{
			.name = "port",
			.parser = barg_int(&port),
			.summary = "Port to listen on",
			.description = "Default value: 0"
		},
		{
			.name = "creation-data",
			.parser = barg_str(&options.creation_data),
			.summary = "User-submitted data",
		},
		{
			.name = "extra-data",
			.parser = barg_str(&options.extra_data),
			.summary = "Server-configured extra data",
		},
		{
			.name = "server-address",
			.parser = barg_array(&addresses_barg),
			.repeatable = true,
			.summary = "Public address for the client",
		},
		{
			.name = "broadcast-rate",
			.parser = barg_int(&int_broadcast_rate),
			.summary = "The frequency to broadcast world snapshot",
		},
		{
			.name = "tick-rate",
			.parser = barg_int(&int_tick_rate),
			.summary = "The frequency to update world state",
		},
		{
			.name = "connect-timeout-s",
			.parser = barg_int(&connect_timeout_s),
			.summary = "How much time the client has to connect to the server",
			.description = "This also affects how long the server is allowed to stay idle without players",
		},
		{
			.name = "max-num-players",
			.parser = barg_int(&options.max_num_players),
			.summary = "Maximum number of players",
		},
		barg_opt_help(),
	};
	barg_t barg = {
		.usage = "cn_server [options] <module-path>",
		.summary = "Start the game server",
		.opts = opts,
		.num_opts = sizeof(opts) / sizeof(opts[0]),
		.allow_positional = true,
	};
	barg_result_t result = barg_parse(&barg, argc, argv);
	if (result.status != BARG_OK) {
		barg_print_result(&barg, result, stderr);
		return result.status == BARG_PARSE_ERROR;
	}
	if (result.arg_index != argc - 1) {
		fprintf(stderr, "Module path is required\n");
		return 1;
	}

	stm_setup();

	void* module = dlopen(argv[result.arg_index], RTLD_NOW | RTLD_LOCAL);
	if (module == NULL) { return 1; }
	snetd_t* (*entry_fn)(void) = (snetd_t* (*)(void))dlsym(module, "snetd_entry");
	if (entry_fn == NULL) {
		fprintf(stderr, "Module does not export a snetd_entry function\n");
		dlclose(module);
		return 1;
	}
	snetd_t* snetd = entry_fn();
	if (snetd == NULL) {
		fprintf(stderr, "snetd_entry has an invalid return value\n");
		dlclose(module);
		return 1;
	}

	buf_t recv_buf = { 0 };
	int cmd_size;

	cn_server_config_t config = cn_server_config_defaults();
	cn_crypto_sign_keygen(&config.public_key, &config.secret_key);
	cn_server_t* server = cn_server_create(config);
	cn_server_start(server, "0.0.0.0:0");
	int bound_port = cn_server_get_endpoint(server).port;

	endpoint_str_t address_bufs[SNETD_MAX_NUM_ADDRESSES + 1] = { 0 };
	const char* address_list[SNETD_MAX_NUM_ADDRESSES + 1] = { 0 };
	int num_addresses = addresses_barg.num_elements;
	for (int i = 0; i < num_addresses; ++i) {
		snprintf(address_bufs[i], sizeof(address_bufs[i]), "%s:%d", addresses[i], bound_port);
		address_list[i] = address_bufs[i];
	}
	// Workaround for cute_net address check
	snprintf(
		address_bufs[num_addresses],
		sizeof(address_bufs[num_addresses]),
		"0.0.0.0:%d", bound_port
	);
	address_list[num_addresses] = address_bufs[num_addresses];
	++num_addresses;

	snetd_env_impl_t env = {
		.env = {
			.realloc = snetd_env_realloc_impl,
			.log = snetd_env_log_impl,
			.send = snetd_env_send_impl,
			.kick = snetd_env_kick_impl,
			.terminate = snetd_env_terminate_impl,
			.allow_join = snetd_env_allow_join_impl,
			.forbid_join = snetd_env_forbid_join_impl,
		},
		.server = server,
		.should_run = true,
	};
	void* snetd_ctx = snetd->init(&env.env, &options);

	uint64_t tick_timer = 0;
	uint64_t tick_count = 0;
	double tick_accumulator_ms = 0.0;
	const double tick_interval_ms = 1000.0 / (double)int_tick_rate;

	uint64_t broadcast_timer = 0;
	double broadcast_accumulator_ms = 0.0;
	const double broadcast_interval_ms = 1000.0 / (double)int_broadcast_rate;

	uint64_t server_update_timer = 0;

	uint64_t poll_timer = 0;
	double time_budget_ms = 0.0;

	uint64_t disconnect_time = time(NULL) + connect_timeout_s;

	while (env.should_run) {
		uint64_t current_unix_time = time(NULL);
		if (env.num_players == 0 && current_unix_time > disconnect_time) {
			snetd_log(&env.env, "Shutting down empty server");
			break;
		}

		stm_laptime(&poll_timer);
		time_budget_ms += tick_interval_ms;

		// Tick
		{
			// TODO: only tick after the first player joined
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
		{
			cn_server_update(server, stm_sec(stm_laptime(&server_update_timer)), current_unix_time);

			cn_server_event_t event;
			while (cn_server_pop_event(server, &event)) {
				switch (event.type) {
					case CN_SERVER_EVENT_TYPE_NEW_CONNECTION: {
						const char* username = (const char*)(uintptr_t)event.u.new_connection.client_id;
						int player_index = alloc_player_slot(&env, username);
						if (player_index >= 0) {
							env.players[player_index] = (player_info_t){
								.username = username,
								.tranport_type = TRANSPORT_TYPE_CUTE_NET,
								.transport_handle = event.u.new_connection.client_index,
							};
							env.cn_to_snet[event.u.new_connection.client_index] = player_index;

							snetd->event(snetd_ctx, &(snetd_event_t){
								.type = SNETD_EVENT_PLAYER_JOINED,
								.player_joined = {
									.username = username,
									.player_index = player_index,
								},
							});
						} else {
							cn_server_disconnect_client(server, event.u.new_connection.client_index, true);
						}
					} break;
					case CN_SERVER_EVENT_TYPE_DISCONNECTED: {
						int player_index = env.cn_to_snet[event.u.disconnected.client_index];
						if (env.players[player_index].tranport_type != TRANSPORT_TYPE_CUTE_NET) {
							continue;
						}

						snetd->event(snetd_ctx, &(snetd_event_t){
							.type = SNETD_EVENT_PLAYER_LEFT,
							.player_left = {
								.player_index = player_index,
							},
						});

						env.players[player_index].username = NULL;
						if (--env.num_players == 0) {
							disconnect_time = current_unix_time + connect_timeout_s;
						}
					} break;
					case CN_SERVER_EVENT_TYPE_PAYLOAD_PACKET: {
						int player_index = env.cn_to_snet[event.u.disconnected.client_index];
						if (env.players[player_index].tranport_type == TRANSPORT_TYPE_CUTE_NET) {
							snetd->event(snetd_ctx, &(snetd_event_t){
								.type = SNETD_EVENT_MESSAGE,
								.message = {
									.sender = player_index,
									.data = event.u.payload_packet.data,
									.size = event.u.payload_packet.size,
								},
							});
						}
						cn_server_free_packet(server, event.u.payload_packet.client_index, event.u.payload_packet.data);
					} break;
				}
			}
		}

		// Use the rest of the timeslice for polling
		time_budget_ms -= stm_ms(stm_laptime(&poll_timer));
		do {
			int poll_result;
			int timeout = time_budget_ms >= 1.0 ? (int)time_budget_ms : 0;
			do {
				struct pollfd polls = {
					.fd = 3,
					.events = POLLIN,
				};
				poll_result = poll(&polls, 1, timeout);
				timeout = 0;

				if ((polls.revents & POLLERR) | (polls.revents & POLLHUP) | (polls.revents & POLLNVAL)) {
					goto end;
				} else if (polls.revents & POLLIN) {
					if ((cmd_size = recv_cmd(&recv_buf)) <= 0) {
						goto end;
					}

					switch (recv_buf.ptr[0]) {
						case PORT_CMD_REQUEST_CONNECT_TOKEN: {
							env.allow_join = false;
							env.rejection_reason = "default_reject";
							if (env.num_players < SNETD_MAX_NUM_PLAYERS) {
								snetd->event(snetd_ctx, &(snetd_event_t){
									.type = SNETD_EVENT_PLAYER_JOINING,
									.player_joining = {
										.username = recv_buf.ptr + 1,
									},
								});
							} else {
								env.rejection_reason = "server_full";
							}

							if (env.allow_join) {
								uint8_t connect_token[CN_CONNECT_TOKEN_SIZE];
								uint64_t unix_timestamp = current_unix_time;
								cn_crypto_key_t client_to_server = cn_crypto_generate_key();
								cn_crypto_key_t server_to_client = cn_crypto_generate_key();
								cn_generate_connect_token(
									0,
									unix_timestamp,
									&client_to_server, &server_to_client,
									unix_timestamp + connect_timeout_s,
									connect_timeout_s,
									num_addresses, address_list,
									(uint64_t)sintern(recv_buf.ptr + 1),
									NULL,
									&config.secret_key,
									connect_token
								);

								send_control_header(PORT_MSG_QUERY_RESPONSE, CN_CONNECT_TOKEN_SIZE + 1);
								send_payload(&env.allow_join, 1);
								send_payload(connect_token, sizeof(connect_token));
							} else {
								int reason_length = strlen(env.rejection_reason);
								send_control_header(PORT_MSG_QUERY_RESPONSE, reason_length + 1);
								send_payload(&env.allow_join, 1);
								send_payload(env.rejection_reason, reason_length);
							}
						} break;
						case PORT_CMD_REQUEST_JOIN_PERMISSION: {
							env.allow_join = false;
							env.rejection_reason = "default_reject";
							if (env.num_players < SNETD_MAX_NUM_PLAYERS) {
								snetd->event(snetd_ctx, &(snetd_event_t){
									.type = SNETD_EVENT_PLAYER_JOINING,
									.player_joining = {
										.username = recv_buf.ptr + 1,
									},
								});
							} else {
								env.rejection_reason = "server_full";
							}

							if (env.allow_join) {
								send_control_header(PORT_MSG_QUERY_RESPONSE, 1);
								send_payload(&env.allow_join, 1);
							} else {
								int reason_length = strlen(env.rejection_reason);
								send_control_header(PORT_MSG_QUERY_RESPONSE, reason_length + 1);
								send_payload(&env.allow_join, 1);
								send_payload(env.rejection_reason, reason_length);
							}
						} break;
						case PORT_CMD_WEBTRANSPORT_CONNECT: {
							const char* username = sintern(recv_buf.ptr + 1);
							int player_index = alloc_player_slot(&env, username);
							if (player_index >= 0) {
								env.players[player_index] = (player_info_t){
									.username = username,
									.tranport_type = TRANSPORT_TYPE_WEBTRANSPORT,
									.transport_handle = player_index,
								};
								snetd->event(snetd_ctx, &(snetd_event_t){
									.type = SNETD_EVENT_PLAYER_JOINED,
									.player_joined = {
										.username = username,
										.player_index = player_index,
									},
								});
								char resp[2] = { 1, player_index };
								send_control_message(PORT_MSG_QUERY_RESPONSE, resp, sizeof(resp));
							} else {
								send_control_message(PORT_MSG_QUERY_RESPONSE, &(char){ 0 }, 1);
							}
						} break;
						case PORT_CMD_WEBTRANSPORT_DISCONNECT: {
							int player_index = recv_buf.ptr[1];
							if (env.players[player_index].tranport_type != TRANSPORT_TYPE_WEBTRANSPORT) {
								continue;
							}

							snetd->event(snetd_ctx, &(snetd_event_t){
								.type = SNETD_EVENT_PLAYER_LEFT,
								.player_left = {
									.player_index = player_index,
								},
							});

							env.players[player_index].username = NULL;
							if (--env.num_players == 0) {
								disconnect_time = current_unix_time + connect_timeout_s;
							}
						} break;
						case PORT_CMD_WEBTRANSPORT_MESSAGE: {
							int player_index = recv_buf.ptr[1];
							if (env.players[player_index].tranport_type != TRANSPORT_TYPE_WEBTRANSPORT) {
								continue;
							}

							snetd->event(snetd_ctx, &(snetd_event_t){
								.type = SNETD_EVENT_MESSAGE,
								.message = {
									.data = &recv_buf.ptr[2],
									.size = cmd_size - 2,
									.sender = player_index,
								},
							});
						} break;
						default: {
							fprintf(stderr, "Invalid command type: %d\r\n", recv_buf.ptr[0]);
							goto end;
						} break;
					}
				}
			} while (poll_result > 0);

			time_budget_ms -= stm_ms(stm_laptime(&poll_timer));
		} while (time_budget_ms >= 1.0);
	}

end:
	cn_server_stop(server);
	cn_server_destroy(server);

	snetd->cleanup(snetd_ctx);
	dlclose(module);

	free(recv_buf.ptr);

	return 0;
}
