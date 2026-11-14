//! One writer at a time: a sync waits for whoever holds the store, here this test process.

use crate::common::{Seed, TestContext, parse_virtual};

#[tokio::test]
async fn a_sync_waits_for_the_store_lock() {
	let ctx = TestContext::build();
	let vi = parse_virtual("- [ ] Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n");
	let issue = ctx.consensus(&vi, Some(Seed::new(-100))).await;
	ctx.remote(
		&parse_virtual("- [ ] Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  remote body\n"),
		Some(Seed::new(100)),
	);
	let commits = || ctx.git_commit_count();
	let before = commits();

	let held = tedi_task_operations::local::lock_store().await.unwrap();
	std::thread::scope(|s| {
		let run = s.spawn(|| ctx.open_issue(&issue).run());
		std::thread::sleep(std::time::Duration::from_millis(800));
		assert!(!run.is_finished(), "the sync went ahead while the store was held");
		assert_eq!(commits(), before);
		drop(held);
		let out = run.join().unwrap();
		assert!(out.status.success(), "stderr: {}", out.stderr);
	});
	assert_eq!(commits(), before + 1, "the sync committed once the store was free");
}
