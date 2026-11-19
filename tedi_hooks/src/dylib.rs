//! A hook in a shared library, called over the C ABI of `abi.rs`.

use std::path::Path;

use crate::{
	Buffer, Ctx, Hook, Moment, Role,
	abi::{TEDI_ABI_VERSION, TediCall, TediHook, TediHost, TediStr},
};

pub(crate) struct Dylib {
	hook: *const TediHook,
	role: Role,
	/// Holds `hook` mapped.
	_lib: libloading::Library,
}
impl Dylib {
	pub(crate) fn load(lib: &Path, symbol: &str, role: Role) -> Result<Self, String> {
		// SAFETY: running a library's initializers is trusting it; only the instance's operator names one
		let lib = unsafe { libloading::Library::new(lib) }.map_err(|e| format!("loading {}: {e}", lib.display()))?;
		// SAFETY: the symbol is read as the ABI says a hook library exports it, and checked for its version before use
		let hook: *const TediHook = unsafe { *lib.get::<*const TediHook>(symbol.as_bytes()).map_err(|e| format!("{symbol}: {e}"))? };
		// SAFETY: a symbol the loader resolved, in a library mapped while `lib` lives
		let declared = unsafe { &*hook };
		if declared.abi_version != TEDI_ABI_VERSION {
			return Err(format!("{symbol} speaks ABI {}, this tedi {TEDI_ABI_VERSION}", declared.abi_version));
		}
		let entry = match role {
			Role::OnEnter => declared.on_enter.is_some(),
			Role::Rule => declared.rule.is_some(),
		};
		if !entry {
			return Err(format!("{symbol} has no {} entry", if role == Role::Rule { "rule" } else { "on_enter" }));
		}
		Ok(Self { hook, role, _lib: lib })
	}
}

// SAFETY: `hook` points into `_lib`'s read-only data, mapped for as long as `_lib` lives; the ABI asks a hook to be callable from any thread
unsafe impl Send for Dylib {}
// SAFETY: as for `Send`: nothing here is written after load
unsafe impl Sync for Dylib {}

static HOST: TediHost = TediHost {
	abi_version: TEDI_ABI_VERSION,
	context,
	edit,
	reject,
	move_to,
};

impl Hook for Dylib {
	fn call(&self, ctx: &Ctx, out: &mut Buffer) -> Result<(), String> {
		let mut context = serde_json::json!({
			"card": ctx.card,
			"params": ctx.params,
			"workflow": ctx.workflow,
			"now": ctx.now,
		});
		match &ctx.at {
			Moment::Entering { from, to, mover } => context["move"] = serde_json::json!({ "from": from, "to": to, "mover": mover }),
			Moment::Pulled { current } => context["current"] = serde_json::json!(current),
		}
		let mut call = TediCall {
			context: context.to_string(),
			entering: matches!(ctx.at, Moment::Entering { .. }),
			edits: Vec::new(),
			rejected: None,
			move_to: None,
			misuse: Vec::new(),
		};
		// SAFETY: `hook` lives as long as `self` (see `Send`)
		let declared = unsafe { &*self.hook };
		let entry = match self.role {
			Role::OnEnter => declared.on_enter,
			Role::Rule => declared.rule,
		}
		.expect("checked present at load");
		// SAFETY: the ABI's contract: both pointers are valid for the call, which doesn't unwind
		unsafe { entry(&HOST, &mut call) };
		if !call.misuse.is_empty() {
			return Err(call.misuse.join("; "));
		}
		for (field, value) in call.edits {
			out.edit(field.parse().map_err(|_| format!("no field {field:?}"))?, value)?;
		}
		out.rejected = call.rejected;
		out.move_to = call.move_to.map(|p| p.parse()).transpose().map_err(|e| format!("move_to: {e}"))?;
		Ok(())
	}
}

/// `None` for bytes that aren't UTF-8.
fn read(s: TediStr) -> Option<String> {
	if s.len == 0 {
		return Some(String::new());
	}
	// SAFETY: the ABI's contract: `ptr` holds `len` bytes for the duration of the call
	let bytes = unsafe { std::slice::from_raw_parts(s.ptr, s.len) };
	String::from_utf8(bytes.to_vec()).ok()
}

extern "C" fn context(call: *const TediCall) -> TediStr {
	// SAFETY: the host handed this pointer to the hook for this call only
	let call = unsafe { &*call };
	TediStr {
		ptr: call.context.as_ptr(),
		len: call.context.len(),
	}
}

extern "C" fn edit(call: *mut TediCall, field: TediStr, value: TediStr) -> bool {
	// SAFETY: as in `context`
	let call = unsafe { &mut *call };
	let (Some(field), Some(value)) = (read(field), read(value)) else {
		call.misuse.push("edit: not UTF-8".into());
		return false;
	};
	let writable = matches!(field.as_str(), "title" | "labels" | "assignees" | "body" | "state");
	if !call.entering || !writable {
		return false;
	}
	call.edits.push((field, value));
	true
}

extern "C" fn reject(call: *mut TediCall, reason: TediStr) {
	// SAFETY: as in `context`
	let call = unsafe { &mut *call };
	match (call.entering, read(reason)) {
		(true, Some(reason)) => call.rejected = Some(reason),
		(false, _) => call.misuse.push("reject: a rule moves or doesn't; only entering refuses".into()),
		(_, None) => call.misuse.push("reject: not UTF-8".into()),
	}
}

extern "C" fn move_to(call: *mut TediCall, path: TediStr) {
	// SAFETY: as in `context`
	let call = unsafe { &mut *call };
	match (call.entering, read(path)) {
		(false, Some(path)) => call.move_to = Some(path),
		(true, _) => call.misuse.push("move_to: only a rule moves".into()),
		(_, None) => call.misuse.push("move_to: not UTF-8".into()),
	}
}
