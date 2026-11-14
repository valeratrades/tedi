//! The kit's class inventory and tokens written where `input.css` reads them (tailwind can't scan a crate
//! unpacked from crates.io), then the client's stylesheet built from it.

fn main() {
	println!("cargo:rerun-if-changed=build.rs");
	println!("cargo:rerun-if-changed=input.css");
	println!("cargo:rerun-if-changed=src/ui");
	if std::env::var_os("CARGO_FEATURE_WEB").is_none() {
		return; // the server ships no stylesheet
	}
	std::fs::write("uikit-classes.txt", ev_lib_classes::CLASS_INVENTORY).expect("the crate dir is writable");
	std::fs::create_dir_all("assets").expect("the crate dir is writable");
	std::fs::write("assets/tokens.css", ev_lib_classes::TOKENS_CSS).expect("the crate dir is writable");
	let built = std::process::Command::new("tailwindcss")
		.args(["--input", "input.css", "--output", "assets/tailwind.css"])
		.status()
		.expect("`tailwindcss` (v4) on PATH: the flake's devshell has it");
	assert!(built.success(), "tailwindcss failed on input.css");
}
