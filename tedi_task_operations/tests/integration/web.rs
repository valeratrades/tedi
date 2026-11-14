//! `tedi web`'s webhook pulls only what `watch` names: the GitHub App is public, so a stranger's install delivers signed with our secret.

use std::{
	io::{Read as _, Write as _},
	net::TcpStream,
	process::{Child, Command, Stdio},
	time::{Duration, Instant},
};

use hmac::Mac as _;

use crate::common::{BINARY_ENV, TestContext, get_binary_path};

const SECRET: &str = "webhook secret";

fn deliver(port: u16, repo: &str) -> u16 {
	let body = format!(r#"{{"repository":{{"full_name":"{repo}"}}}}"#);
	let mut mac = hmac::Hmac::<sha2::Sha256>::new_from_slice(SECRET.as_bytes()).unwrap();
	mac.update(body.as_bytes());
	let signature = hex::encode(mac.finalize().into_bytes());
	let mut stream = TcpStream::connect(("127.0.0.1", port)).unwrap();
	write!(
		stream,
		"POST /webhook HTTP/1.1\r\nHost: board\r\nContent-Type: application/json\r\nX-Hub-Signature-256: sha256={signature}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
		body.len()
	)
	.unwrap();
	let mut response = String::new();
	stream.read_to_string(&mut response).unwrap();
	response.split(' ').nth(1).expect("an HTTP status line").parse().unwrap()
}

#[test]
fn a_delivery_for_a_repo_outside_watch_is_not_pulled() {
	let ctx = TestContext::build();
	std::fs::write(
		ctx.xdg.inner.root.join("config/tedi.toml"),
		format!("watch = [\"o\"]\ngithub_webhook_secret = \"{SECRET}\"\n\n[board]\narchive_after = \"1w\"\nviews = []\n"),
	)
	.unwrap();
	let port = std::net::TcpListener::bind("127.0.0.1:0").unwrap().local_addr().unwrap().port();
	let mut cmd = Command::new(get_binary_path());
	cmd.args(["--mock", "web", "--addr", &format!("127.0.0.1:{port}")]);
	cmd.envs(BINARY_ENV);
	cmd.env(concat!("tedi", "_GITHUB_TOKEN"), "test_token");
	for (key, value) in ctx.xdg.env_vars() {
		cmd.env(key, value);
	}
	cmd.env(concat!("tedi", "_MOCK_STATE"), &ctx.mock_state_path);
	cmd.env(crate::common::ENV_MOCK_CALL_LOG, ctx.call_log_path());
	struct Server(Child);
	impl Drop for Server {
		fn drop(&mut self) {
			self.0.kill().unwrap(); // a failed assert must not leave it serving, holding the harness' stderr open
			self.0.wait().unwrap();
		}
	}
	let mut server = Server(cmd.stdout(Stdio::null()).stderr(Stdio::inherit()).spawn().unwrap());

	let up = Instant::now();
	while TcpStream::connect(("127.0.0.1", port)).is_err() {
		assert!(server.0.try_wait().unwrap().is_none(), "`tedi web` exited before serving");
		assert!(up.elapsed() < Duration::from_secs(30), "`tedi web` never listened");
		std::thread::sleep(Duration::from_millis(20));
	}

	assert_eq!(deliver(port, "stranger/r"), 204);
	assert_eq!(deliver(port, "o/r"), 202);
	// the puller is serial: once the watched repo is listed, the stranger's would have been before it
	let listed = Instant::now();
	while !ctx.github_calls().iter().any(|c| c.starts_with("list_repo_issues(o/r")) {
		assert!(listed.elapsed() < Duration::from_secs(30), "the watched repo was never pulled: {:?}", ctx.github_calls());
		std::thread::sleep(Duration::from_millis(20));
	}
	drop(server);
	assert!(!ctx.github_calls().iter().any(|c| c.contains("stranger")), "{:?}", ctx.github_calls());
}
