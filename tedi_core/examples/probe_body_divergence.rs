//! Sweep for `tmp/ongoing_debug/body_representation_diverges_from_remote.md`: every issue file's
//! body must parse back from its own render. `probe_body_divergence <issues_dir> <current_user>`
fn main() {
	let args: Vec<String> = std::env::args().collect();
	tedi_core::current_user::set(args[2].clone());
	let (mut checked, mut diverged) = (0, 0);
	for entry in walkdir(std::path::Path::new(&args[1])) {
		let content = std::fs::read_to_string(&entry).unwrap();
		let Ok(vi) = tedi_core::VirtualIssue::parse(&content, entry.clone()) else { continue }; // non-issue files (sprints, meta) share the tree
		checked += 1;
		let local = vi.contents.comments.first().expect("body comment always present").body.clone();
		let rendered: String = local.clone().into();
		let reparsed = tedi_core::Events::parse(&rendered);
		if local != reparsed {
			diverged += 1;
			let (l, r) = (local.to_vec(), reparsed.to_vec());
			let i = l.iter().zip(r.iter()).position(|(a, b)| a != b).unwrap_or(l.len().min(r.len()));
			println!("{}\n  at {i}: L {:?}\n         R {:?}", entry.display(), l.get(i), r.get(i));
		}
	}
	println!("{checked} checked, {diverged} diverged");
}

fn walkdir(dir: &std::path::Path) -> Vec<std::path::PathBuf> {
	let mut out = Vec::new();
	for e in std::fs::read_dir(dir).unwrap() {
		let p = e.unwrap().path();
		if p.file_name().is_some_and(|n| n == ".git") {
			continue;
		}
		if p.is_dir() {
			out.extend(walkdir(&p));
		} else if p.extension().is_some_and(|x| x == "md") {
			out.push(p);
		}
	}
	out
}
