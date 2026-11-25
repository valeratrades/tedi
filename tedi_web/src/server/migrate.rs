//! What viewers keep beside the store, brought to the shape this server reads before it serves any: the reading code
//! knows one shape. `version` in the data dir numbers the shape it holds; what a step rewrites stays in `before-{n}`,
//! which it reads from, so a step cut short is run again whole.

use std::{
	collections::{BTreeMap, BTreeSet},
	path::{Path, PathBuf},
};

use color_eyre::eyre::{Result, WrapErr as _, bail, eyre};
use tedi_core::board::{GroupBy, Order, Query, QueryError, Scope, Shown, Slice, Terms, scope_at};

use super::{Pinned, Settings, Shares, Snapshot, Source, items, move_scopes, source_repo, viewed, write_library, write_shares};
use crate::api::{Arrangement, Boxes, Boxing, Columns, Construction, Drafts, Forked, Item, Lens, Library, Owner, ShareLink, Tints, Upstream, View, Workspace};

const VERSION: u32 = 2;

pub(super) async fn migrate(settings: &Settings) -> Result<()> {
	let web = tedi_task_operations::paths::data_dir("web");
	let marker = web.join("version");
	let held: u32 = match std::fs::read_to_string(&marker) {
		Ok(v) => v.trim().parse().wrap_err_with(|| format!("{} numbers no version", marker.display()))?,
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => match jsons(&web)?.is_empty() && jsons(&web.join("shared"))?.is_empty() {
			true => VERSION, // nobody kept anything yet
			false => 0,
		},
		Err(e) => return Err(e.into()),
	};
	if held > VERSION {
		bail!("{} holds version {held}, kept by a later tedi; this one reads {VERSION}", web.display());
	}
	if held < 1 {
		let before = before(&web, 1)?;
		lenses(settings, &before)
			.await
			.wrap_err_with(|| format!("bringing {} to version 1; as it was: {}", web.display(), before.display()))?;
		tracing::info!("{} brought to version 1; as it was: {}", web.display(), before.display());
	}
	if held < 2 {
		let before = before(&web, 2)?;
		orphans(settings, &before).wrap_err_with(|| format!("bringing {} to version 2; as it was: {}", web.display(), before.display()))?;
		tracing::info!("{} brought to version 2; as it was: {}", web.display(), before.display());
	}
	let fresh = web.join("version.new");
	std::fs::write(&fresh, VERSION.to_string())?;
	std::fs::rename(&fresh, &marker)?;
	Ok(())
}

/// The names of `dir`'s `.json` files; none when there is no such dir.
fn jsons(dir: &Path) -> Result<BTreeSet<String>> {
	let entries = match std::fs::read_dir(dir) {
		Ok(entries) => entries,
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(BTreeSet::new()),
		Err(e) => return Err(e.into()),
	};
	let mut out = BTreeSet::new();
	for entry in entries {
		let name = entry?.file_name().into_string().map_err(|n| eyre!("{n:?} in {} is no viewer's", dir.display()))?;
		out.extend(name.strip_suffix(".json").map(str::to_string));
	}
	Ok(out)
}

/// The libraries and shares as they were before step `n`, copied aside once.
fn before(web: &Path, n: u32) -> Result<PathBuf> {
	let before = web.join(format!("before-{n}"));
	if !before.exists() {
		let fresh = web.join(format!(".before-{n}"));
		if fresh.exists() {
			std::fs::remove_dir_all(&fresh)?; // a copy cut short
		}
		for dir in ["", "shared"] {
			std::fs::create_dir_all(fresh.join(dir))?;
			for key in jsons(&web.join(dir))? {
				let file = Path::new(dir).join(format!("{key}.json"));
				std::fs::copy(web.join(&file), fresh.join(&file))?;
			}
		}
		std::fs::rename(&fresh, &before)?;
	}
	Ok(before)
}

/// Step 1: which cards and in what order goes from views to workspaces, how they are boxed from the screen to views.
async fn lenses(settings: &Settings, before: &Path) -> Result<()> {
	let read = |file: PathBuf| std::fs::read_to_string(&file).wrap_err_with(|| file.display().to_string());
	let mut libraries: BTreeMap<String, OldLibrary> = BTreeMap::new();
	for key in jsons(before)? {
		let old = serde_json::from_str(&read(before.join(format!("{key}.json")))?).wrap_err_with(|| format!("{key}'s library"))?;
		libraries.insert(key, old);
	}
	let mut shares: BTreeMap<String, Vec<OldPinned>> = BTreeMap::new();
	for key in jsons(&before.join("shared"))? {
		let old = serde_json::from_str(&read(before.join("shared").join(format!("{key}.json")))?).wrap_err_with(|| format!("{key}'s shares"))?;
		shares.insert(key, old);
	}
	let past = match &settings.source {
		Some(source) => Past::read(source).await?,
		None => Past(Vec::new()), // unversioned: no library numbers a version of the team's
	};
	let fresh: Vec<(String, Library)> = libraries.iter().map(|(key, old)| Ok((key.clone(), old.library(key, settings, &past)?))).collect::<Result<_>>()?;
	let pinned: Vec<(String, Shares)> = shares
		.into_iter()
		.map(|(key, old)| {
			let shares = pins(&key, old, libraries.get(&key), settings, &past);
			(key, shares)
		})
		.collect();
	for (key, library) in &fresh {
		write_library(key, library)?;
	}
	for (key, shares) in &pinned {
		write_shares(key, shares)?;
	}
	Ok(())
}

/// Step 2: the team's scopes by org left its config, points of the chain in their place. A viewer's scope nested in one
/// that is gone sits at the top.
fn orphans(settings: &Settings, before: &Path) -> Result<()> {
	let mut fresh: Vec<(String, Library)> = Vec::new();
	for key in jsons(before)? {
		let file = before.join(format!("{key}.json"));
		let text = std::fs::read_to_string(&file).wrap_err_with(|| file.display().to_string())?;
		let mut library: Library = serde_json::from_str(&text).wrap_err_with(|| format!("{key}'s library"))?;
		let there = |library: &Library, path: &str| library.scopes.contains_key(path) || scope_at(&settings.scopes, path).is_some();
		let orphan = |library: &Library| {
			let mut nested = library.scopes.keys().filter_map(|p| Some((p.clone(), p.rsplit_once(" / ")?)));
			nested.find_map(|(path, (parent, name))| (!there(library, parent)).then(|| (path.clone(), name.to_string())))
		};
		//LOOP each takes one to the top, where it is nested no more
		while let Some((path, name)) = orphan(&library) {
			let to = free(&name, |n| there(&library, n));
			tracing::info!("{key}: the scope {path} is {to} now");
			move_scopes(&mut library, &path, &to);
		}
		let mut favorites = std::mem::take(&mut library.favorites);
		favorites.retain(|f| !matches!(f, Item::Scope(path) if !there(&library, path)));
		library.favorites = favorites;
		fresh.push((key, library));
	}
	for (key, library) in &fresh {
		write_library(key, library)?;
	}
	Ok(())
}

#[derive(Clone, PartialEq, serde::Deserialize)]
struct OldView {
	name: String,
	query: String,
	group_by: Boxing,
	sort: Order,
	shown: Vec<Shown>,
}
impl OldView {
	fn boxed(&self, boxes: Boxes) -> View {
		View {
			name: self.name.clone(),
			group_by: self.group_by.clone(),
			shown: self.shown.clone(),
			boxes,
		}
	}
}

#[derive(Clone, PartialEq, serde::Deserialize)]
struct OldColumns {
	by: GroupBy,
	#[serde(default)]
	layout: Arrangement,
	#[serde(default)]
	tints: Tints,
}
impl From<OldColumns> for Columns {
	fn from(old: OldColumns) -> Self {
		Columns {
			by: old.by,
			boxes: Boxes {
				arrangement: old.layout,
				tints: old.tints,
				..Default::default()
			},
		}
	}
}

#[derive(serde::Deserialize)]
struct OldScreen {
	tiles: Arrangement,
	#[serde(default)]
	views: BTreeMap<String, OldView>,
	columns: BTreeMap<String, BTreeMap<GroupBy, Arrangement>>,
	#[serde(default)]
	tints: BTreeMap<String, BTreeMap<GroupBy, Tints>>,
	#[serde(default)]
	laid: BTreeMap<String, BTreeMap<GroupBy, String>>,
	#[serde(default)]
	locked: BTreeSet<String>,
}

#[derive(serde::Deserialize)]
struct OldLibrary {
	#[serde(default)]
	scope: Option<String>,
	#[serde(default)]
	workspace: Option<String>,
	on_screen: OldScreen,
	filters: BTreeMap<String, OldView>,
	workspaces: BTreeMap<String, Arrangement>,
	columns: BTreeMap<String, OldColumns>,
	#[serde(default)]
	scopes: BTreeMap<String, Scope>,
	favorites: BTreeSet<Item>,
	#[serde(default)]
	ids: BTreeMap<String, Item>,
	#[serde(default)]
	forks: Vec<Forked>,
	#[serde(default)]
	recent: Vec<String>,
	#[serde(default)]
	autosaved: Option<jiff::Timestamp>,
	#[serde(default)]
	received: Vec<String>,
}
impl OldLibrary {
	/// The terms of the scope the screen sat in.
	fn terms(&self, who: &str, settings: &Settings, past: &Past) -> Terms {
		let Some(path) = &self.scope else {
			return Terms::default();
		};
		let own = Library {
			scopes: self.scopes.clone(),
			..Default::default()
		};
		let tree = own.scopes(&settings.scopes).unwrap_or_default(); // one lost its parent: the old board served them nothing either
		let left = || self.scopes.get(path).cloned().or_else(|| past.scope(path));
		match scope_at(&tree, path).cloned().or_else(left) {
			Some(scope) => scope.filter(),
			None => {
				tracing::warn!("{who}: the scope {path} is no longer there: the workspaces narrow by none");
				Terms::default()
			}
		}
	}

	fn library(&self, who: &str, settings: &Settings, past: &Past) -> Result<Library> {
		let screen = &self.on_screen;
		let scope = self.terms(who, settings, past);
		let shown = |name: &str| screen.views.get(name).or_else(|| self.filters.get(name)).cloned().or_else(|| past.view(name));
		let workspace = |tiles: &Arrangement| Workspace {
			tiles: tiles.clone(),
			lens: lens(who, scope.clone(), tiles, shown),
		};

		let mut workspaces: BTreeMap<String, Workspace> = self.workspaces.iter().map(|(name, tiles)| (name.clone(), workspace(tiles))).collect();
		let mut drafts = Drafts::default();
		let left = |kept: &Arrangement| {
			let mut tiles = kept.clone();
			tiles.extend(screen.tiles.clone());
			workspace(&tiles)
		};
		let open = match &self.workspace {
			Some(name) if let Some(kept) = self.workspaces.get(name) => {
				workspaces.insert(name.clone(), left(kept));
				name.clone()
			}
			Some(name) if let Some(theirs) = settings.workspaces.get(name) => {
				drafts.workspaces.insert(name.clone(), left(&theirs.tiles));
				name.clone()
			}
			_ => {
				let taken = |n: &String| workspaces.contains_key(n) || settings.workspaces.contains_key(n);
				let name = (1..).map(|n| format!("ws {n}")).find(|n| !taken(n)).expect("only so many are kept");
				workspaces.insert(name.clone(), workspace(&screen.tiles));
				name
			}
		};

		let mut scopes = self.scopes.clone();
		for view in self.filters.values() {
			match view.query.parse::<Terms>() {
				Ok(terms) if terms == Terms::default() => {}
				Ok(terms) => {
					let name = free(&view.name.replace(" / ", " - "), |n| scopes.contains_key(n) || settings.scopes.contains_key(n));
					scopes.insert(name, Scope::from(terms));
				}
				Err(QueryError::Text(_)) => {
					// a scope holds no words, a search does
					let name = free(&view.name, |n| workspaces.contains_key(n) || settings.workspaces.contains_key(n));
					let lens = Lens {
						scope: Slice::Terms(scope.clone()),
						search: view.query.parse().expect("only its words make it no terms"),
						sort: view.sort.clone(),
					};
					workspaces.insert(name, Workspace { tiles: Arrangement::new(), lens });
				}
				Err(e) => tracing::warn!("{who}: the query of {} never ran and is left behind: {e}", view.name),
			}
		}

		let boxes = |name: &str, by: &Boxing| match by {
			Boxing::Groups(by) => Boxes {
				arrangement: screen.columns.get(name).and_then(|c| c.get(by)).cloned().unwrap_or_default(), // never arranged
				tints: screen.tints.get(name).and_then(|t| t.get(by)).cloned().unwrap_or_default(),         // never tinted
				..Default::default()
			},
			Boxing::Chain => Boxes::default(), // no column was ever boxed by it
		};
		let views: BTreeMap<String, View> = self.filters.iter().map(|(name, v)| (name.clone(), v.boxed(boxes(name, &v.group_by)))).collect();
		for name in screen.views.keys().chain(screen.columns.keys()).chain(screen.tints.keys()).collect::<BTreeSet<_>>() {
			let Some(kept) = views.get(name).or_else(|| settings.views.iter().find(|v| v.name == *name)) else {
				continue; // gone since
			};
			let mut draft = match screen.views.get(name) {
				Some(edited) => edited.boxed(Boxes::default()),
				None => kept.clone(),
			};
			draft.boxes = boxes(name, &draft.group_by);
			if draft.group_by == kept.group_by {
				draft.boxes.hidden = kept.boxes.hidden.clone();
			}
			if draft != *kept {
				drafts.views.insert(name.clone(), draft);
			}
		}

		let grouping = |name: &str| {
			drafts
				.views
				.get(name)
				.or_else(|| views.get(name))
				.or_else(|| settings.views.iter().find(|v| v.name == name))
				.and_then(|v| if let Boxing::Groups(by) = &v.group_by { Some(by.clone()) } else { None })
		};
		let laid = screen.laid.iter().filter_map(|(name, by)| Some((name.clone(), by.get(&grouping(name)?)?.clone()))).collect();
		let forks = self
			.forks
			.iter()
			.map(|f| Forked {
				item: f.item.clone(),
				upstream: match (&f.upstream.owner, f.upstream.version) {
					(Owner::Team, Some(v)) => Upstream {
						version: past.renumbered(&Item::new(f.item.shelf(), f.upstream.name.clone()), v),
						..f.upstream.clone()
					},
					_ => f.upstream.clone(),
				},
			})
			.collect();
		let received = self.received.iter().map(|l| link(l, past)).collect::<Result<_>>()?;
		Ok(Library {
			autosaved: self.autosaved.map(|at| (open.clone(), at)).into_iter().collect(),
			workspace: Some(open),
			views,
			workspaces,
			columns: self.columns.iter().map(|(n, c)| (n.clone(), c.clone().into())).collect(),
			scopes,
			drafts,
			laid,
			locked: screen.locked.clone(),
			favorites: self.favorites.clone(),
			ids: self.ids.clone(),
			forks,
			recent: self.recent.clone(),
			received,
		})
	}
}

/// What the view shown in the largest of `tiles` showing one narrowed and ordered by, in the widest band arranged;
/// else the first behind what is shown.
fn lens(who: &str, scope: Terms, tiles: &Arrangement, view: impl Fn(&str) -> Option<OldView>) -> Lens {
	let shown = tiles.values().rev().find_map(|layout| {
		shown_in(layout).unwrap_or_else(|e| {
			tracing::warn!("{who}: {e}"); // kept as it is: opening it says so
			None
		})
	});
	let fresh = Lens {
		scope: Slice::Terms(scope),
		..Default::default()
	};
	let Some(view) = shown.and_then(|name| view(&name)) else {
		return fresh; // it shows none, or one that is gone
	};
	match view.query.parse::<Query>() {
		Ok(search) => Lens { search, sort: view.sort, ..fresh },
		Err(e) => {
			tracing::warn!("{who}: the query of {} never ran and is left behind: {e}", view.name);
			Lens { sort: view.sort, ..fresh }
		}
	}
}

fn shown_in(layout: &str) -> Result<Option<String>> {
	let mut grid = tilers_core::model::serial::load(layout).map_err(|e| eyre!("a kept layout doesn't load: {e:?}"))?;
	grid.cells.sort_by_key(|c| std::cmp::Reverse(c.w * c.h));
	let up = grid.cells.iter().find_map(|c| c.group.active_panel().0.strip_prefix("view:").map(str::to_string));
	Ok(up.or(viewed(layout)?.into_iter().next()))
}

fn free(name: &str, taken: impl Fn(&String) -> bool) -> String {
	let numbered = (2..).map(|n| format!("{name} ({n})"));
	std::iter::once(name.to_string()).chain(numbered).find(|n| !taken(n)).expect("only so many are kept")
}

/// A link without its scope, to the same thing as the team's versions are numbered now.
fn link(old: &str, past: &Past) -> Result<ShareLink> {
	let (path, query) = old.split_once('?').unwrap_or((old, ""));
	let query: Vec<&str> = query.split('&').filter(|p| !p.is_empty() && !p.starts_with("scope=")).collect();
	let link = if query.is_empty() { path.to_string() } else { format!("{path}?{}", query.join("&")) };
	let mut link: ShareLink = link.parse().map_err(|e: String| eyre!(e))?;
	if link.owner == Owner::Team {
		link.version = past.renumbered(&link.item, link.version).unwrap_or(link.version); // of no commit held: it pins nothing either way
	}
	Ok(link)
}

#[derive(serde::Deserialize)]
struct OldPinned {
	item: Item,
	versions: Vec<OldSnapshot>,
}
#[derive(serde::Deserialize)]
struct OldSnapshot {
	construction: OldConstruction,
	carried: BTreeMap<String, u32>,
}
#[derive(Clone, serde::Deserialize)]
enum OldConstruction {
	View(OldView),
	Scope(Scope),
	Workspace(Arrangement),
	Columns(OldColumns),
}

fn pins(who: &str, old: Vec<OldPinned>, library: Option<&OldLibrary>, settings: &Settings, past: &Past) -> Shares {
	let scope = library.map(|l| l.terms(who, settings, past)).unwrap_or_default(); // a sharer who keeps nothing any more
	let pinned: BTreeMap<String, Vec<OldConstruction>> = old
		.iter()
		.filter_map(|p| match &p.item {
			Item::View(name) => Some((name.clone(), p.versions.iter().map(|s| s.construction.clone()).collect())),
			_ => None,
		})
		.collect();
	let construction = |s: &OldSnapshot| -> Construction {
		match s.construction.clone() {
			OldConstruction::View(view) => Construction::View(view.boxed(Boxes::default())),
			OldConstruction::Scope(scope) => Construction::Scope(scope),
			OldConstruction::Columns(columns) => Construction::Columns(columns.into()),
			OldConstruction::Workspace(tiles) => {
				let shown = |name: &str| match s.carried.get(name) {
					Some(v) => match pinned.get(name)?.get((*v as usize).checked_sub(1)?)? {
						OldConstruction::View(view) => Some(view.clone()),
						_ => None,
					},
					None => past.view(name),
				};
				Construction::Workspace(Workspace {
					lens: lens(who, scope.clone(), &tiles, shown),
					tiles,
				})
			}
		}
	};
	let snapshot = |s: &OldSnapshot| Snapshot {
		construction: construction(s),
		carried: s.carried.clone(),
	};
	Shares(
		old.iter()
			.map(|p| Pinned {
				item: p.item.clone(),
				versions: p.versions.iter().map(snapshot).collect(),
			})
			.collect(),
	)
}

/// The team's config at each commit, oldest first.
struct Past(Vec<Commit>);
impl Past {
	async fn read(source: &Source) -> Result<Self> {
		#[derive(serde::Deserialize)]
		struct File {
			board: Written,
		}
		#[derive(serde::Deserialize)]
		struct Written {
			#[serde(default)] // a commit may hold none
			views: Vec<toml::Value>,
			#[serde(default)] // a commit may hold none
			columns: BTreeMap<String, OldColumns>,
		}
		let commits = tedi_task_operations::github::client::get()?
			.file_history(source_repo(source)?, &source.path, &source.branch)
			.await?;
		let mut out = Vec::new();
		for text in commits.into_iter().filter_map(|c| c.content) {
			let written = toml::from_str::<File>(&text)?.board;
			out.push(Commit {
				views: written.views,
				columns: written.columns,
				read: items(&super::team_of(&text)?),
			});
		}
		Ok(Past(out))
	}

	fn written(views: &[toml::Value], name: &str) -> Option<OldView> {
		let view = views.iter().find(|v| v.get("name").and_then(|n| n.as_str()) == Some(name))?;
		view.clone().try_into().ok() // written since views lost what they narrowed by
	}

	/// The team's, as it last narrowed.
	fn view(&self, name: &str) -> Option<OldView> {
		self.0.iter().rev().find_map(|c| Self::written(&c.views, name))
	}

	/// The team's, as it last was.
	fn scope(&self, path: &str) -> Option<Scope> {
		let item = Item::Scope(path.to_string());
		self.0.iter().rev().find_map(|c| {
			if let Some(Construction::Scope(scope)) = c.read.get(&item) {
				Some(scope.clone())
			} else {
				None
			}
		})
	}

	/// Version `v` of `item`, as numbered when a view's query and order made versions of it.
	fn renumbered(&self, item: &Item, v: u32) -> Option<u32> {
		let (mut old, mut new) = (0, 0);
		let (mut last_view, mut last_columns, mut last_read, mut last) = (None, None, None, None::<&Construction>);
		for Commit { views, columns, read } in &self.0 {
			let moved = match item {
				Item::View(name) => Self::written(views, name).is_some_and(|w| last_view.replace(w.clone()) != Some(w)),
				Item::Columns(name) => columns.get(name).is_some_and(|c| last_columns.replace(c) != Some(c)),
				_ => read.get(item).is_some_and(|c| last_read.replace(c) != Some(c)),
			};
			old += u32::from(moved);
			new += u32::from(read.get(item).is_some_and(|c| last.replace(c) != Some(c)));
			if old == v {
				return Some(new);
			}
		}
		None
	}
}

struct Commit {
	/// As written.
	views: Vec<toml::Value>,
	columns: BTreeMap<String, OldColumns>,
	/// As it reads now.
	read: BTreeMap<Item, Construction>,
}
