//! One writer at a time over the store, from a merge to its consensus commit: across processes (the
//! CLI, a web server) through a flock on `issues/.lock`, across tasks of one process through a mutex
//! in front of it — a second flock from the same process would wait on itself.

use std::sync::atomic::{AtomicUsize, Ordering};

use super::Local;

static IN_PROCESS: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());
static HELD: AtomicUsize = AtomicUsize::new(0);

/// Relative to the issues dir; consensus commits never take it.
pub const LOCK_FILE: &str = ".lock";

#[must_use = "the store is unlocked once this drops"]
pub struct StoreLock {
	_file: std::fs::File,
	_in_process: tokio::sync::MutexGuard<'static, ()>,
}
impl Drop for StoreLock {
	fn drop(&mut self) {
		HELD.fetch_sub(1, Ordering::SeqCst);
	}
}

/// Waits for whoever holds it. Never take it while already holding it.
pub async fn lock_store() -> std::io::Result<StoreLock> {
	let in_process = IN_PROCESS.lock().await;
	let path = Local::issues_dir().join(LOCK_FILE);
	std::fs::create_dir_all(path.parent().expect("the lock lives in the issues dir"))?;
	let file = std::fs::OpenOptions::new().create(true).write(true).truncate(false).open(path)?;
	let file = tokio::task::spawn_blocking(move || file.lock().map(|()| file)).await.expect("taking a flock doesn't panic")?;
	HELD.fetch_add(1, Ordering::SeqCst);
	Ok(StoreLock {
		_file: file,
		_in_process: in_process,
	})
}

/// Every consensus commit asserts it.
pub(crate) fn store_locked() -> bool {
	HELD.load(Ordering::SeqCst) > 0
}
