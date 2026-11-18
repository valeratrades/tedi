//! `tedi.h` is what hook authors build against: it must say what `src/abi.rs` does.

#[test]
fn the_header_matches_the_abi() {
	let dir = env!("CARGO_MANIFEST_DIR");
	let config = cbindgen::Config::from_file(format!("{dir}/cbindgen.toml")).expect("cbindgen.toml parses");
	let mut generated = Vec::new();
	cbindgen::Builder::new()
		.with_crate(dir)
		.with_config(config)
		.generate()
		.expect("the ABI generates")
		.write(&mut generated);
	let generated = String::from_utf8(generated).expect("cbindgen writes UTF-8");
	let path = format!("{dir}/tedi.h");
	if std::env::var_os("TEDI_BLESS").is_some() {
		std::fs::write(&path, &generated).unwrap();
	}
	let held = std::fs::read_to_string(&path).expect("tedi.h is checked in");
	assert!(held == generated, "tedi.h is stale: `TEDI_BLESS=1 cargo test -p tedi_hooks --test header` regenerates it");
}
