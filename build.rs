fn main() {
	git_version();
	log_directives();
}

fn git_version() {
	// a dev build's script never reruns for a commit: each rerun recompiles the package and relinks its tests
	if std::env::var("PROFILE").as_deref() != Ok("release") {
		println!("cargo:rerun-if-changed=build.rs");
		println!("cargo:rustc-env=GIT_HASH=dev");
		return;
	}
	// resolved by git: the package dir need not be the repo root, and in a worktree `.git` is a file.
	// This checkout's HEAD reflog moves on its every commit/checkout/reset; `refs/` is shared by all worktrees, so watching it rebuilt every checkout on any agent's commit.
	if let Some(paths) = std::process::Command::new("git")
		.args(["rev-parse", "--git-path", "HEAD", "--git-path", "logs/HEAD"])
		.output()
		.ok()
		.filter(|o| o.status.success())
	{
		// a missing path makes cargo rerun the script on every build
		// shortcut: without a reflog (core.logAllRefUpdates=false) a commit leaves HEAD untouched and GIT_HASH goes stale, upgrade if a repo ever turns reflogs off
		for path in String::from_utf8_lossy(&paths.stdout).lines().filter(|p| std::path::Path::new(p).exists()) {
			println!("cargo:rerun-if-changed={path}");
		}
	}
	// Embed git commit hash (fallback to "unknown" if git unavailable, e.g., in Nix sandbox)
	let git_hash = std::process::Command::new("git")
		.args(["rev-parse", "--short", "HEAD"])
		.output()
		.ok()
		.and_then(|o| if o.status.success() { Some(o.stdout) } else { None })
		.and_then(|stdout| String::from_utf8(stdout).ok())
		.map(|s| s.trim().to_string())
		.unwrap_or_else(|| "unknown".to_string());
	println!("cargo:rustc-env=GIT_HASH={git_hash}");
}

fn log_directives() {
	// a missing path named here leaves the script dirty on every build: without `.cargo`, the package dir (cargo's own default) is watched for it to appear
	println!("cargo:rerun-if-changed={}", if std::path::Path::new(".cargo").exists() { ".cargo" } else { "." });
	if let Ok(directives) = std::fs::read_to_string(".cargo/log_directives") {
		let directives = directives.trim();
		if !directives.is_empty() {
			println!("cargo:rustc-env=LOG_DIRECTIVES={directives}");
		}
	}
}
