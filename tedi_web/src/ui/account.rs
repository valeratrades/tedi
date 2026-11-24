//! The viewer's account: keys to GitHub, and the scopes of it stored for them.

use dioxus::prelude::*;

use super::{
	Ctx,
	card::ago,
	icons::{Glyph, Icon},
	kit::{Btn, IconBtn, Style},
};
use crate::api::{self, Account, AccountEdit, Outcome};

const FIELD: &str = "h-7 min-w-0 rounded-[var(--control-radius)] border border-border px-2 text-mono text-ink outline-none focus:border-ring placeholder:font-sans placeholder:text-body-s placeholder:text-ink-faint";

#[component]
pub(super) fn AccountPanel() -> Element {
	let ctx = use_context::<Ctx>();
	let mut account = use_resource(move || {
		_ = ctx.board.read(); // reread on every change announced: a pull's end is one
		async move { api::account().await }
	});
	let mut refused = use_signal(|| None::<String>);
	let (mut owner, mut token, mut scope) = (use_signal(String::new), use_signal(String::new), use_signal(String::new));
	let close = move || {
		let mut m = ctx.modal;
		m.set(None);
	};
	let edit = move |edit: AccountEdit, landed: Callback<()>| {
		spawn(async move {
			match api::edit_account(edit).await {
				Ok(_) => {
					refused.set(None);
					landed.call(());
					account.restart();
					if let Ok(now) = api::setup().await {
						let mut setup = ctx.setup;
						setup.set(Some(now)); // who the viewer is, is their keys'
					}
				}
				Err(e) => refused.set(Some(e.to_string())),
			}
		});
	};
	let nothing = use_callback(|()| {});
	let keyed = use_callback(move |()| {
		owner.set(String::new());
		token.set(String::new());
	});
	let scoped = use_callback(move |()| scope.set(String::new()));
	let body = match &*account.read() {
		None => rsx! { span { class: "text-body text-ink-soft", "…" } },
		Some(Err(e)) => rsx! { span { class: "text-body text-accent-error", "{e}" } },
		Some(Ok(Account { login, keys, scopes })) => rsx! {
			Part { title: "Keys", about: "A GitHub token per owner: private repos need it, and what you do there is done as you. It stays on the server.",
				for k in keys.iter() {
					div { key: "{k.owner}", class: "flex h-7 items-center gap-2",
						span { class: "text-mono text-ink", "{k.owner}" }
						if let Some(login) = login { span { class: "text-caption text-ink-soft", "as {login}" } }
						div { class: "flex-1" }
						Ended { outcome: k.outcome.clone() }
						IconBtn { glyph: Glyph::X, title: "Drop the key on {k.owner}", onclick: { let owner = k.owner.clone(); move |_| edit(AccountEdit::DropKey(owner.clone()), nothing) } }
					}
				}
				form {
					class: "flex items-center gap-2",
					onsubmit: move |e: FormEvent| {
						e.prevent_default();
						edit(AccountEdit::Key { owner: owner(), token: token() }, keyed);
					},
					input { class: "w-36 {FIELD}", aria_label: "Key's owner", placeholder: "owner", value: owner(), oninput: move |e: FormEvent| owner.set(e.value()) }
					input { class: "flex-1 {FIELD}", aria_label: "Token", r#type: "password", autocomplete: "off", placeholder: "token", value: token(), oninput: move |e: FormEvent| token.set(e.value()) }
					Btn { style: Style::Outline, md: true, disabled: owner.read().trim().is_empty() || token.read().trim().is_empty(), onclick: move |_| edit(AccountEdit::Key { owner: owner(), token: token() }, keyed), "Add key" }
				}
			}
			Part { title: "Scopes", about: "Stored here beside the team's, and yours alone to see: as far as your own key lists them.",
				for s in scopes.iter() {
					div { key: "{s.scope}", class: "flex h-7 items-center gap-2",
						span { class: "text-mono text-ink", "{s.scope}" }
						span { class: "text-caption text-ink-soft", if s.repos == 1 { "1 repo" } else { "{s.repos} repos" } }
						div { class: "flex-1" }
						Ended { outcome: s.outcome.clone() }
						IconBtn { glyph: Glyph::X, title: "Drop the scope {s.scope}", onclick: { let scope = s.scope.clone(); move |_| edit(AccountEdit::DropScope(scope.clone()), nothing) } }
					}
				}
				form {
					class: "flex items-center gap-2",
					onsubmit: move |e: FormEvent| {
						e.prevent_default();
						edit(AccountEdit::Scope(scope()), scoped);
					},
					input { class: "flex-1 {FIELD}", aria_label: "Scope", placeholder: "owner, or owner/repo", value: scope(), oninput: move |e: FormEvent| scope.set(e.value()) }
					Btn { style: Style::Outline, md: true, disabled: scope.read().trim().is_empty(), onclick: move |_| edit(AccountEdit::Scope(scope()), scoped), "Add scope" }
				}
			}
		},
	};
	let me = ctx.me();
	rsx! {
		div { class: "fixed inset-0 z-[100] flex items-center justify-center bg-scrim p-4", onclick: move |_| close(),
			div {
				role: "dialog",
				aria_label: "Account",
				class: "flex max-h-full w-[560px] max-w-full flex-col gap-4 overflow-y-auto rounded-[10px] border border-border bg-popover px-5 py-4 text-ink",
				onclick: |e: MouseEvent| e.stop_propagation(),
				onkeydown: move |e: KeyboardEvent| if e.key() == Key::Escape { close() },
				div { class: "flex h-6 items-center gap-2",
					Icon { glyph: Glyph::User, class: "size-4 text-ink-soft" }
					span { class: "text-h3 text-ink", "Account" }
					if let Some(me) = me { span { class: "text-caption text-ink-soft", "{me}" } }
					div { class: "flex-1" }
					IconBtn { glyph: Glyph::X, title: "Close", onclick: move |_| close() }
				}
				if let Some(why) = refused() { div { role: "alert", class: "text-body-s text-accent-error", "{why}" } }
				{body}
			}
		}
	}
}

#[component]
fn Part(title: String, about: String, children: Element) -> Element {
	rsx! {
		div { class: "flex flex-col gap-1.5",
			span { class: "text-body-s font-medium text-ink-mid", "{title}" }
			{children}
			span { class: "text-caption text-ink-soft", "{about}" }
		}
	}
}

/// How the last pull ended.
#[component]
fn Ended(outcome: Option<Outcome>) -> Element {
	match outcome {
		None => rsx! { span { class: "text-caption text-ink-faint", "not pulled yet" } },
		Some(Outcome { at, error: None }) => rsx! { span { class: "text-caption text-ink-soft", title: "{at}", "pulled {ago(at)}" } },
		Some(Outcome { at, error: Some(error) }) => rsx! { span { class: "max-w-60 truncate text-caption text-accent-error", title: "{at}: {error}", "{error}" } },
	}
}
