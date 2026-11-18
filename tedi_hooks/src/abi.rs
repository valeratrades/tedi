//! The C ABI a hook library speaks. `tedi.h` is generated from this file.

/// A library built against another version is refused at load.
pub const TEDI_ABI_VERSION: u32 = 1;

/// UTF-8, not NUL-terminated. Handed out by the host, it lives until the hook returns; handed in, the
/// host copies it before returning.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct TediStr {
	pub ptr: *const u8,
	pub len: usize,
}

/// One call of a hook, opaque to it.
pub struct TediCall {
	pub(crate) context: String,
	pub(crate) entering: bool,
	pub(crate) edits: Vec<(String, String)>,
	pub(crate) rejected: Option<String>,
	pub(crate) move_to: Option<String>,
	/// What the hook did that its call doesn't allow; fails the move.
	pub(crate) misuse: Vec<String>,
}

/// What the host offers a hook. Writes only buffer: nothing reaches the store before the hook returns,
/// and then only if every hook of the move agreed.
#[repr(C)]
pub struct TediHost {
	pub abi_version: u32,
	/// JSON: `card` (as the board shows it), `params` (the hook's own, from the workflow file),
	/// `workflow`, `now`; entering, `move` (`from`, `to`, `mover`: `{"person": login}` or `"rule"`);
	/// in a rule, `current`.
	pub context: extern "C" fn(call: *const TediCall) -> TediStr,
	/// Entering: sets `field` (`title`, `labels`, `assignees`, `body`, `state`, written as `tedi set`
	/// takes them) to `value`, against the card shown. False when the field isn't one of those, or in a rule.
	pub edit: extern "C" fn(call: *mut TediCall, field: TediStr, value: TediStr) -> bool,
	/// Entering: refuses the move, saying why. Nothing of it lands.
	pub reject: extern "C" fn(call: *mut TediCall, reason: TediStr),
	/// In a rule: moves the card to the leaf at `path` (`/`-joined bucket names).
	pub move_to: extern "C" fn(call: *mut TediCall, path: TediStr),
}

/// What a library exports, under the symbol its workflow names. An entry the workflow uses it for
/// must be set. Neither may unwind: a Rust hook catches panics at this boundary (`catch_unwind`).
#[repr(C)]
pub struct TediHook {
	pub abi_version: u32,
	/// When a move enters a bucket listing it under `on_enter`.
	pub on_enter: Option<unsafe extern "C" fn(host: *const TediHost, call: *mut TediCall)>,
	/// When a pull stores the card, for a workflow listing it under `rules`.
	pub rule: Option<unsafe extern "C" fn(host: *const TediHost, call: *mut TediCall)>,
}
