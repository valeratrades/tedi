// A hook in C: entering the bucket that names it closes the issue as completed.
//
//   cc -shared -fPIC -I tedi_hooks -o libclose.so tedi_hooks/examples/close.c
//
// and in a workflow file:  on_enter = [{ lib = "libclose.so", symbol = "close_on_enter" }]

#include <string.h>

#include "tedi.h"

static TediStr str(const char *s) {
	return (TediStr){(const uint8_t *)s, strlen(s)};
}

static void on_enter(const TediHost *host, TediCall *call) {
	// `context` is JSON; a hook reading the card would parse it. This one closes whatever enters.
	host->edit(call, str("state"), str("x"));
}

const TediHook close_on_enter = {TEDI_ABI_VERSION, on_enter, NULL};
