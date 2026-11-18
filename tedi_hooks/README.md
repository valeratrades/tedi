Workflow hooks: what entering a bucket does, and how a pull moves a card. A workflow file names each hook
beside its parameters:

```toml
[[buckets]]
name = "done"
on_enter = [{ builtin = "close", reason = "completed" }, { lib = "libmine.so", symbol = "my_hook", any = "param" }]

[[rules]]
builtin = "on_pr"
state = "open"
to = "in_review"
forward_only = true
```

Builtins: `on_enter` — `assign_mover`, `close{reason}`, `reopen`, `reject{reason}` (refuses people, never rules);
rules — `on_pr{state}`, `on_closed{reason}`, `on_reopened`, `closed_for{after}`, each with `to`, `forward_only`
and `from` (bucket paths it may move out of). Presets in `presets/` are written the same way.

## A hook library
Any language with a C ABI: include [`tedi.h`](tedi.h), export a `TediHook` under the symbol the workflow names
([`examples/close.c`](examples/close.c)). The host loads it once at startup and checks its `abi_version` against
its own; a mismatch is refused there. A change to `src/abi.rs` bumps `TEDI_ABI_VERSION`.

- **Reads** are JSON (`host->context`): the card as the board shows it, the hook's params, the workflow, now,
  and the move (entering) or the current state (a rule).
- **Writes buffer.** `edit` / `reject` / `move_to` record what the hook asks; the host applies it after every hook
  of the move returned — all of its edits land, made against the card each hook was shown, or none do.
- **No unwinding** across the boundary: a Rust hook wraps its body in `catch_unwind`.
- Strings the host hands out live until the hook returns.
