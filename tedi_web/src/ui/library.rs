//! ☰: what the viewer keeps on one shelf — favorites first, a click takes one, and the name typed on top
//! saves the screen as it, or renames an entry to it.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::*;

use super::Ctx;
use crate::api::{Favorite, LibraryEdit, Shelf};

#[component]
pub fn Menu(shelf: Shelf, names: Vec<String>, on_pick: EventHandler<String>, on_save: Option<EventHandler<String>>) -> Element {
	let ctx = use_context::<Ctx>();
	let mut open = use_signal(|| false);
	let mut typed = use_signal(String::new);
	let favorite = move |name: &str| match shelf {
		Shelf::Filters => Favorite::View(name.to_string()),
		Shelf::Workspaces => Favorite::Workspace(name.to_string()),
		Shelf::Columns => Favorite::Columns(name.to_string()),
	};
	let mut names = names;
	names.sort_by_key(|n| !ctx.favorite(&favorite(n)));
	let name = typed.read().trim().to_string();
	let edit = move |edit: LibraryEdit| {
		spawn(async move {
			if ctx.keep(edit).await {
				typed.set(String::new());
			}
		});
	};
	rsx! {
		// inside the phone's sheet a click closes it; one in here is the menu's own
		div { onclick: |e: MouseEvent| e.stop_propagation(),
			Popover { open: open(), on_open_change: move |v| open.set(v),
				PopoverTrigger { class: "px-2 text-sm text-ink-soft hover:text-ink", aria_label: format!("{shelf:?}"), "☰" }
				PopoverContent { align: "start", class: "flex w-72 flex-col gap-2 p-2",
					Input { size: Size::Sm, value: typed(), placeholder: "name", oninput: move |e: FormEvent| typed.set(e.value()) }
					if let Some(save) = on_save {
						Button {
							variant: ButtonVariant::Outline,
							size: Size::Sm,
							r#type: "button",
							disabled: name.is_empty(),
							onclick: {
								let name = name.clone();
								move |_| {
									save.call(name.clone());
									typed.set(String::new());
								}
							},
							if names.contains(&name) { "Save over {name}" } else { "Save as {name}" }
						}
					}
					if names.is_empty() {
						span { class: "text-xs text-ink-soft", "Nothing kept yet" }
					}
					for n in names {
						div { key: "{n}", class: "flex items-center gap-1 text-sm",
							{
								let starred = ctx.favorite(&favorite(&n));
								let what = favorite(&n);
								rsx! {
									button {
										class: if starred { "px-1 text-accent" } else { "px-1 text-ink-soft" },
										title: if starred { "Unfavorite" } else { "Favorite" },
										onclick: move |_| edit(LibraryEdit::Favorite { what: what.clone(), on: !starred }),
										if starred { "★" } else { "☆" }
									}
								}
							}
							button {
								class: "flex-1 truncate text-left hover:underline",
								onclick: {
									let n = n.clone();
									move |_| {
										open.set(false);
										on_pick.call(n.clone());
									}
								},
								"{n}"
							}
							button {
								class: "px-1 text-ink-soft hover:text-ink disabled:opacity-30",
								title: "Rename to the name above",
								disabled: name.is_empty() || name == n,
								onclick: {
									let (from, to) = (n.clone(), name.clone());
									move |_| edit(LibraryEdit::Rename { shelf, from: from.clone(), to: to.clone() })
								},
								"✎"
							}
							button {
								class: "px-1 text-ink-soft hover:text-ink",
								title: "Delete",
								onclick: { let n = n.clone(); move |_| edit(LibraryEdit::Drop { shelf, name: n.clone() }) },
								"✕"
							}
						}
					}
				}
			}
		}
	}
}
