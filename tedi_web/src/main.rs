//! The client bundle: `dx serve` / `dx bundle` build it; `tedi web` serves it and the API it calls.

use dioxus::prelude::*;

fn main() {
	// a bundle built with `--base-path` calls its server functions under it, which `dioxus/fullstack` would
	// arrange and the client goes without (see Cargo.toml); the url parses as absolute
	if let Some(base) = dioxus::cli_config::base_path() {
		let origin = web_sys::window().expect("runs in a browser").location().origin().expect("a page has an origin");
		dioxus_fullstack::set_server_url(format!("{origin}/{}", base.trim_matches('/')).leak());
	}
	dioxus::launch(|| {
		rsx! {
			document::Title { "tedi" }
			tedi_web::ui::App {}
		}
	});
}

#[used]
static TAILWIND: Asset = asset!("/assets/tailwind.css", AssetOptions::css().with_static_head(true));
#[used]
static LOADING: Asset = asset!("/src/loading.js", AssetOptions::js().with_static_head(true).with_minify(false));
