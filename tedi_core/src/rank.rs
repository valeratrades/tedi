//! Rank, the sort key of the board (sort descending).
//!
//! Over the "x blocks y" graph of **open** issues (a closed issue and every edge that
//! touches it are ignored):
//!
//! - `E(x) = max(i(x), E(y) for each y that x blocks)`: importance flows up to blockers.
//! - `d(x) = 1 + max d(y) over y that x blocks`, and `d = 0` when x blocks nothing.
//! - `Rank = E·10⁴ + min(d, 999)·10 + i`.
//!
//! So when x blocks y, `E(x) ≥ E(y)` and on a tie `d(x) > d(y)`: x sorts above y. The
//! `·10` step outweighs any `i` difference (at most 9), and `·10⁴` outweighs the depth
//! term (at most 9 999). The invariant holds while depth stays under 999.
//!
//! A cycle has no such order, so a strongly connected component (Tarjan) is folded into
//! one node with the highest importance of its members; [`Ranking::cycles`] reports the
//! members so the caller can write a Sync note on each.
//!
//! The whole graph costs O(V + E), so it is recomputed on every read.

use std::collections::BTreeMap;

pub type Rank = u32;

/// What rank needs to know about one issue.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Task {
	/// The `i:` value, 0…9. The caller decides what an issue without `i:` counts as.
	pub importance: u8,
	pub open: bool,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Ranking<K> {
	/// One entry per open issue; closed issues have none and keep whatever Rank they had.
	pub ranks: BTreeMap<K, Rank>,
	/// Each cycle's members, sorted; cycles sorted by their first member.
	pub cycles: Vec<Vec<K>>,
}

/// `blocks` holds `(x, y)` pairs meaning "x blocks y". An edge to a key that is not in
/// `tasks` is ignored like an edge to a closed issue: it is outside what the board shows.
pub fn rank<K: Clone + Ord>(tasks: &BTreeMap<K, Task>, blocks: &[(K, K)]) -> Ranking<K> {
	let keys: Vec<&K> = tasks.iter().filter(|(_, task)| task.open).map(|(key, _)| key).collect();
	let index: BTreeMap<&K, usize> = keys.iter().enumerate().map(|(i, key)| (*key, i)).collect();
	let importance: Vec<u32> = keys.iter().map(|key| u32::from(tasks[*key].importance.min(9))).collect();

	let mut successors = vec![Vec::new(); keys.len()];
	let mut self_loop = vec![false; keys.len()];
	for (x, y) in blocks {
		let (Some(&x), Some(&y)) = (index.get(x), index.get(y)) else { continue };
		if x == y {
			self_loop[x] = true;
		} else {
			successors[x].push(y);
		}
	}

	let components = tarjan(&successors);
	let mut component_of = vec![0; keys.len()];
	for (c, members) in components.iter().enumerate() {
		for &v in members {
			component_of[v] = c;
		}
	}

	// Tarjan emits a component only after every component it reaches, so the
	// successors of each one are already computed when it comes up.
	let mut effective = vec![0u32; components.len()];
	let mut depth = vec![0u32; components.len()];
	for (c, members) in components.iter().enumerate() {
		let mut e = members.iter().map(|&v| importance[v]).max().unwrap_or(0);
		let mut d = None;
		for &v in members {
			for &w in &successors[v] {
				let s = component_of[w];
				if s != c {
					e = e.max(effective[s]);
					d = Some(d.map_or(depth[s], |d: u32| d.max(depth[s])));
				}
			}
		}
		effective[c] = e;
		depth[c] = d.map_or(0, |d| d + 1);
	}

	let ranks = keys
		.iter()
		.enumerate()
		.map(|(v, key)| {
			let c = component_of[v];
			((*key).clone(), effective[c] * 10_000 + depth[c].min(999) * 10 + importance[v])
		})
		.collect();

	let mut cycles: Vec<Vec<K>> = components
		.iter()
		.filter(|members| members.len() > 1 || self_loop[members[0]])
		.map(|members| {
			let mut keys: Vec<K> = members.iter().map(|&v| keys[v].clone()).collect();
			keys.sort();
			keys
		})
		.collect();
	cycles.sort();

	Ranking { ranks, cycles }
}

/// Strongly connected components, each emitted after every component it can reach.
/// Iterative, so a long chain of blockers cannot overflow the stack.
fn tarjan(successors: &[Vec<usize>]) -> Vec<Vec<usize>> {
	const UNVISITED: usize = usize::MAX;
	let n = successors.len();
	let mut order = vec![UNVISITED; n];
	let mut low = vec![0; n];
	let mut on_stack = vec![false; n];
	let mut stack = Vec::new();
	let mut components = Vec::new();
	let mut next = 0;

	for root in 0..n {
		if order[root] != UNVISITED {
			continue;
		}
		// (node, the next successor of it to look at)
		let mut calls = vec![(root, 0)];
		order[root] = next;
		low[root] = next;
		next += 1;
		stack.push(root);
		on_stack[root] = true;

		while let Some(&(v, edge)) = calls.last() {
			if let Some(&w) = successors[v].get(edge) {
				calls.last_mut().expect("the loop condition saw a frame").1 += 1;
				if order[w] == UNVISITED {
					order[w] = next;
					low[w] = next;
					next += 1;
					stack.push(w);
					on_stack[w] = true;
					calls.push((w, 0));
				} else if on_stack[w] {
					low[v] = low[v].min(order[w]);
				}
				continue;
			}
			calls.pop();
			if let Some(&(parent, _)) = calls.last() {
				low[parent] = low[parent].min(low[v]);
			}
			if low[v] == order[v] {
				let mut members = Vec::new();
				loop {
					let w = stack.pop().expect("v is still on the stack");
					on_stack[w] = false;
					members.push(w);
					if w == v {
						break;
					}
				}
				components.push(members);
			}
		}
	}
	components
}

#[cfg(test)]
mod tests {
	use super::*;

	fn open(importance: u8) -> Task {
		Task { importance, open: true }
	}

	fn closed(importance: u8) -> Task {
		Task { importance, open: false }
	}

	fn graph(tasks: &[(&'static str, Task)]) -> BTreeMap<&'static str, Task> {
		tasks.iter().copied().collect()
	}

	fn r(e: u32, d: u32, i: u32) -> Rank {
		e * 10_000 + d * 10 + i
	}

	#[test]
	fn isolated_node_is_its_own_importance() {
		let ranking = rank(&graph(&[("a", open(4))]), &[]);
		assert_eq!(ranking.ranks, BTreeMap::from([("a", r(4, 0, 4))]));
		assert!(ranking.cycles.is_empty());
	}

	#[test]
	fn chain_blocker_inherits_importance() {
		// a blocks b
		let ranking = rank(&graph(&[("a", open(2)), ("b", open(7))]), &[("a", "b")]);
		assert_eq!(ranking.ranks["a"], r(7, 1, 2));
		assert_eq!(ranking.ranks["b"], r(7, 0, 7));
		assert!(ranking.ranks["a"] > ranking.ranks["b"]);
	}

	#[test]
	fn depth_three_chain() {
		// a blocks b blocks c blocks d
		let tasks = graph(&[("a", open(1)), ("b", open(1)), ("c", open(1)), ("d", open(5))]);
		let ranking = rank(&tasks, &[("a", "b"), ("b", "c"), ("c", "d")]);
		assert_eq!(ranking.ranks["d"], r(5, 0, 5));
		assert_eq!(ranking.ranks["c"], r(5, 1, 1));
		assert_eq!(ranking.ranks["b"], r(5, 2, 1));
		assert_eq!(ranking.ranks["a"], r(5, 3, 1));
	}

	#[test]
	fn diamond_takes_the_longest_path_and_the_highest_importance() {
		// a blocks b and c; b and c block d; c also blocks e
		let tasks = graph(&[("a", open(1)), ("b", open(3)), ("c", open(2)), ("d", open(4)), ("e", open(8))]);
		let ranking = rank(&tasks, &[("a", "b"), ("a", "c"), ("b", "d"), ("c", "d"), ("c", "e")]);
		assert_eq!(ranking.ranks["d"], r(4, 0, 4));
		assert_eq!(ranking.ranks["e"], r(8, 0, 8));
		assert_eq!(ranking.ranks["b"], r(4, 1, 3));
		assert_eq!(ranking.ranks["c"], r(8, 1, 2));
		assert_eq!(ranking.ranks["a"], r(8, 2, 1));
	}

	#[test]
	fn cycle_is_folded_and_reported() {
		// a → b → c → a
		let tasks = graph(&[("a", open(1)), ("b", open(6)), ("c", open(3))]);
		let ranking = rank(&tasks, &[("a", "b"), ("b", "c"), ("c", "a")]);
		assert_eq!(ranking.cycles, vec![vec!["a", "b", "c"]]);
		// One node: E is the members' highest, d is shared, each keeps its own i.
		assert_eq!(ranking.ranks["a"], r(6, 0, 1));
		assert_eq!(ranking.ranks["b"], r(6, 0, 6));
		assert_eq!(ranking.ranks["c"], r(6, 0, 3));
	}

	#[test]
	fn self_block_is_a_cycle() {
		let ranking = rank(&graph(&[("a", open(2))]), &[("a", "a")]);
		assert_eq!(ranking.cycles, vec![vec!["a"]]);
		assert_eq!(ranking.ranks["a"], r(2, 0, 2));
	}

	#[test]
	fn cycle_feeding_a_chain() {
		// x blocks the cycle {a, b}; the cycle blocks c, which blocks d.
		let tasks = graph(&[("x", open(0)), ("a", open(2)), ("b", open(3)), ("c", open(1)), ("d", open(9))]);
		let ranking = rank(&tasks, &[("x", "a"), ("a", "b"), ("b", "a"), ("b", "c"), ("c", "d")]);
		assert_eq!(ranking.cycles, vec![vec!["a", "b"]]);
		assert_eq!(ranking.ranks["d"], r(9, 0, 9));
		assert_eq!(ranking.ranks["c"], r(9, 1, 1));
		assert_eq!(ranking.ranks["a"], r(9, 2, 2));
		assert_eq!(ranking.ranks["b"], r(9, 2, 3));
		assert_eq!(ranking.ranks["x"], r(9, 3, 0));
	}

	#[test]
	fn closed_blocker_is_ignored() {
		// closed a blocks b: b is as if unblocked, a has no rank.
		let tasks = graph(&[("a", closed(9)), ("b", open(2)), ("c", open(1))]);
		let ranking = rank(&tasks, &[("a", "b"), ("c", "a")]);
		assert_eq!(ranking.ranks, BTreeMap::from([("b", r(2, 0, 2)), ("c", r(1, 0, 1))]));
	}

	#[test]
	fn closed_issue_breaks_a_cycle() {
		let tasks = graph(&[("a", open(1)), ("b", open(2)), ("c", closed(3))]);
		let ranking = rank(&tasks, &[("a", "b"), ("b", "c"), ("c", "a")]);
		assert!(ranking.cycles.is_empty());
		assert!(ranking.ranks["a"] > ranking.ranks["b"]);
	}

	#[test]
	fn edge_to_an_unknown_issue_is_ignored() {
		let ranking = rank(&graph(&[("a", open(3))]), &[("a", "elsewhere"), ("elsewhere", "a")]);
		assert_eq!(ranking.ranks["a"], r(3, 0, 3));
	}

	#[test]
	fn depth_is_capped_at_999() {
		let names: Vec<String> = (0..1_200).map(|n| format!("{n:05}")).collect();
		let tasks: BTreeMap<String, Task> = names.iter().map(|n| (n.clone(), open(0))).collect();
		let blocks: Vec<(String, String)> = names.windows(2).map(|w| (w[0].clone(), w[1].clone())).collect();
		let ranking = rank(&tasks, &blocks);
		assert_eq!(ranking.ranks[&names[0]], r(0, 999, 0));
		assert_eq!(ranking.ranks[&names[1_199]], r(0, 0, 0));
	}

	/// A tiny LCG: the property test needs reproducible noise, not good randomness.
	struct Lcg(u64);

	impl Lcg {
		fn next(&mut self, bound: u64) -> u64 {
			self.0 = self.0.wrapping_mul(6_364_136_223_846_793_005).wrapping_add(1_442_695_040_888_963_407);
			(self.0 >> 33) % bound
		}
	}

	#[test]
	fn blocker_ranks_above_blocked_on_random_dags() {
		let mut rng = Lcg(0x5eed);
		for _ in 0..500 {
			let n = 1 + rng.next(40) as usize;
			let tasks: BTreeMap<usize, Task> = (0..n)
				.map(|k| {
					let task = Task {
						importance: rng.next(10) as u8,
						open: rng.next(5) != 0,
					};
					(k, task)
				})
				.collect();
			// Edges only from a lower position to a higher one in a shuffled order: a DAG.
			let mut position: Vec<usize> = (0..n).collect();
			for k in (1..n).rev() {
				position.swap(k, rng.next(k as u64 + 1) as usize);
			}
			let mut blocks = Vec::new();
			for a in 0..n {
				for b in a + 1..n {
					if rng.next(6) == 0 {
						blocks.push((position[a], position[b]));
					}
				}
			}
			let ranking = rank(&tasks, &blocks);
			assert!(ranking.cycles.is_empty());
			for (x, y) in &blocks {
				if let (Some(rx), Some(ry)) = (ranking.ranks.get(x), ranking.ranks.get(y)) {
					assert!(rx > ry, "{x} blocks {y} but ranks {rx} ≤ {ry} in {blocks:?}");
				}
			}
		}
	}

	#[test]
	fn invariant_holds_across_components_of_random_graphs() {
		let mut rng = Lcg(0xc0ffee);
		for _ in 0..500 {
			let n = 1 + rng.next(25) as usize;
			let tasks: BTreeMap<usize, Task> = (0..n).map(|k| (k, open(rng.next(10) as u8))).collect();
			let blocks: Vec<(usize, usize)> = (0..rng.next(3 * n as u64 + 1)).map(|_| (rng.next(n as u64) as usize, rng.next(n as u64) as usize)).collect();
			let ranking = rank(&tasks, &blocks);
			let cycle_of = |k: usize| ranking.cycles.iter().position(|members| members.contains(&k));
			for &(x, y) in &blocks {
				if x != y && (cycle_of(x).is_none() || cycle_of(x) != cycle_of(y)) {
					assert!(ranking.ranks[&x] > ranking.ranks[&y], "{x} blocks {y} in {blocks:?}");
				}
			}
		}
	}
}
