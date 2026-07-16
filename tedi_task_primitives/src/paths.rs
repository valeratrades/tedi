//! App-identity paths. The runtime lives under the `tedi` app name regardless of
//! which crate the IO code compiles in — the `v_utils` xdg macros would otherwise
//! bake in `CARGO_PKG_NAME` (`tedi_task_primitives`) and split the app's data directory.

use std::path::{Path, PathBuf};

use xdg::BaseDirectories;

const APP: &str = "tedi";

/// `$XDG_DATA_HOME/tedi/<sub>/`
pub fn data_dir(sub: &str) -> PathBuf {
	dirs().create_data_directory(sub).unwrap()
}
/// `$XDG_CACHE_HOME/tedi/<name>`
pub fn cache_file(name: &str) -> PathBuf {
	file(name, |d, parent| d.create_cache_directory(parent).unwrap())
}
/// `$XDG_STATE_HOME/tedi/<name>`
pub fn state_file(name: &str) -> PathBuf {
	file(name, |d, parent| d.create_state_directory(parent).unwrap())
}
fn dirs() -> BaseDirectories {
	BaseDirectories::with_prefix(APP)
}

fn file(name: &str, mk: impl Fn(&BaseDirectories, &Path) -> PathBuf) -> PathBuf {
	let p = Path::new(name);
	let parent = p.parent().unwrap_or(Path::new(""));
	mk(&dirs(), parent).join(p.file_name().unwrap())
}
