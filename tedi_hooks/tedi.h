#ifndef TEDI_H
#define TEDI_H

/* Generated from tedi_hooks/src/abi.rs by cbindgen; `TEDI_BLESS=1 cargo test -p tedi_hooks --test header` regenerates it. */

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// A library built against another version is refused at load.
#define TEDI_ABI_VERSION 1

// One call of a hook, opaque to it.
typedef struct TediCall TediCall;

// UTF-8, not NUL-terminated. Handed out by the host, it lives until the hook returns; handed in, the
// host copies it before returning.
typedef struct TediStr {
  const uint8_t *ptr;
  size_t len;
} TediStr;

// What the host offers a hook. Writes only buffer: nothing reaches the store before the hook returns,
// and then only if every hook of the move agreed.
typedef struct TediHost {
  uint32_t abi_version;
  // JSON: `card` (as the board shows it), `params` (the hook's own, from the workflow file),
  // `workflow`, `now`; entering, `move` (`from`, `to`, `mover`: `{"person": login}` or `"rule"`);
  // in a rule, `current`.
  struct TediStr (*context)(const struct TediCall *call);
  // Entering: sets `field` (`title`, `labels`, `assignees`, `body`, `state`, written as `tedi set`
  // takes them) to `value`, against the card shown. False when the field isn't one of those, or in a rule.
  bool (*edit)(struct TediCall *call, struct TediStr field, struct TediStr value);
  // Entering: refuses the move, saying why. Nothing of it lands.
  void (*reject)(struct TediCall *call, struct TediStr reason);
  // In a rule: moves the card to the leaf at `path` (`/`-joined bucket names).
  void (*move_to)(struct TediCall *call, struct TediStr path);
} TediHost;

// What a library exports, under the symbol its workflow names. An entry the workflow uses it for
// must be set. Neither may unwind: a Rust hook catches panics at this boundary (`catch_unwind`).
typedef struct TediHook {
  uint32_t abi_version;
  // When a move enters a bucket listing it under `on_enter`.
  void (*on_enter)(const struct TediHost *host, struct TediCall *call);
  // When a pull stores the card, for a workflow listing it under `rules`.
  void (*rule)(const struct TediHost *host, struct TediCall *call);
} TediHook;

#endif  /* TEDI_H */
