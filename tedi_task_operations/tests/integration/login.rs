//! `tedi login`: the browser is sent where the home's gate says, comes back to a loopback port with a code, and what the
//! code is exchanged for rides on every call to the home.

use std::{
	io::{BufRead as _, BufReader, Read as _, Write as _},
	net::{TcpListener, TcpStream},
	path::Path,
};

use serde_json::{Value, json};
use sha2::Digest as _;

use crate::common::{BINARY_ENV, TestContext, get_binary_path};

const SESSION: &str = "a-session";

/// One request's line, `Authorization` and body.
fn request(stream: &mut TcpStream) -> (String, String, String) {
	let mut reader = BufReader::new(stream);
	let mut first = String::new();
	reader.read_line(&mut first).unwrap();
	let (mut authorization, mut length) = (String::new(), 0);
	loop {
		let mut line = String::new();
		reader.read_line(&mut line).unwrap();
		let Some((name, value)) = line.trim().split_once(": ") else { break };
		match name.to_lowercase().as_str() {
			"authorization" => authorization = value.to_string(),
			"content-length" => length = value.parse().unwrap(),
			_ => {}
		}
	}
	let mut body = vec![0; length];
	reader.read_exact(&mut body).unwrap();
	(first.trim().to_string(), authorization, String::from_utf8(body).unwrap())
}

fn answer(stream: &mut TcpStream, status: &str, headers: &str, body: &str) {
	write!(
		stream,
		"HTTP/1.1 {status}\r\n{headers}Content-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
		body.len()
	)
	.unwrap();
}

/// A home at `/board` behind a gate that signs in at `/auth/cli`, serving as many requests as named.
fn gated(listener: TcpListener, requests: usize, verifiers: std::sync::mpsc::Sender<String>) {
	let uri = format!("http://{}/auth/cli", listener.local_addr().unwrap());
	for _ in 0..requests {
		let (mut stream, _) = listener.accept().unwrap();
		let (line, authorization, body) = request(&mut stream);
		match line.as_str() {
			"POST /auth/cli/token HTTP/1.1" => {
				let body: Value = serde_json::from_str(&body).unwrap();
				assert_eq!(body["code"], "a-code");
				verifiers.send(body["code_verifier"].as_str().unwrap().to_string()).unwrap();
				answer(&mut stream, "200 OK", "", &json!({ "session": SESSION }).to_string());
			}
			"POST /board/api/move HTTP/1.1" if authorization == format!("Bearer {SESSION}") => answer(&mut stream, "200 OK", "", "\"Landed\""),
			line if line.starts_with("POST /board/api/") => answer(&mut stream, "401 Unauthorized", &format!("WWW-Authenticate: Bearer authorization_uri=\"{uri}\"\r\n"), ""),
			line => panic!("{line}"),
		}
	}
}

fn fetch(port: &str, path: &str) -> String {
	let mut stream = TcpStream::connect(format!("127.0.0.1:{port}")).unwrap();
	write!(stream, "GET {path} HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n").unwrap();
	let mut said = String::new();
	stream.read_to_string(&mut said).unwrap();
	said
}

#[test]
fn a_browser_brings_the_session_the_home_then_takes() {
	let ctx = TestContext::build();
	let listener = TcpListener::bind("127.0.0.1:0").unwrap();
	let home = format!("http://{}/board", listener.local_addr().unwrap());
	ctx.xdg.write_config("config.toml", &format!("github_token = \"test_token\"\nhome = \"{home}\"\n"));
	let (told, verifiers) = std::sync::mpsc::channel();
	let gate = std::thread::spawn(move || gated(listener, 4, told));
	let move_it = || ctx.run(&["--mock", "move", "https://github.com/o/r/issues/1", "kanban/selected"]);

	let out = move_it();
	assert!(!out.status.success() && out.stderr.contains("tedi login"), "{}", out.stderr);

	// the browser: what `xdg-open` is called with, fetched
	let opened = ctx.xdg.inner.root.join("opened");
	let bin = ctx.xdg.inner.root.join("bin");
	std::fs::create_dir(&bin).unwrap();
	std::fs::write(bin.join("xdg-open"), format!("#!/bin/sh\nprintf %s \"$1\" > {0}.new && mv {0}.new {0}\n", opened.display())).unwrap();
	std::fs::set_permissions(bin.join("xdg-open"), std::os::unix::fs::PermissionsExt::from_mode(0o755)).unwrap();
	let mut cmd = std::process::Command::new(get_binary_path());
	cmd.args(["--mock", "login"]).envs(BINARY_ENV).envs(ctx.xdg.env_vars());
	cmd.env("PATH", format!("{}:{}", bin.display(), std::env::var("PATH").unwrap()));
	let mut login = cmd.spawn().unwrap();
	let url = opened_url(&opened);
	let param = |name: &str| url.split(['?', '&']).find_map(|p| p.strip_prefix(&format!("{name}="))).unwrap().to_string();
	assert_eq!(param("code_challenge_method"), "S256");
	let redirect = param("redirect_uri").replace("%3A", ":").replace("%2F", "/");
	let (port, path) = redirect.strip_prefix("http://127.0.0.1:").unwrap().split_once('/').unwrap();
	assert!(fetch(port, &format!("/{path}?code=forged&state=another")).starts_with("HTTP/1.1 404"), "another's state");
	assert!(fetch(port, &format!("/{path}?code=a-code&state={}", param("state"))).starts_with("HTTP/1.1 200"));
	assert!(login.wait().unwrap().success());

	let kept = ctx.xdg.inner.root.join("state/tedi/session.json");
	let kept = if kept.exists() { kept } else { ctx.xdg.state_dir().join("session.json") };
	assert_eq!(std::os::unix::fs::PermissionsExt::mode(&std::fs::metadata(&kept).unwrap().permissions()) & 0o777, 0o600);
	let challenge = base64::Engine::encode(&base64::engine::general_purpose::URL_SAFE_NO_PAD, sha2::Sha256::digest(verifiers.recv().unwrap()));
	assert_eq!(param("code_challenge"), challenge, "the code is exchanged by whoever asked for it");

	let out = move_it();
	assert!(out.status.success(), "{}", out.stderr);
	gate.join().unwrap();
}

fn opened_url(at: &Path) -> String {
	let since = std::time::Instant::now();
	while !at.exists() {
		assert!(since.elapsed() < std::time::Duration::from_secs(30), "no browser was opened");
		std::thread::sleep(std::time::Duration::from_millis(20));
	}
	std::fs::read_to_string(at).unwrap()
}
