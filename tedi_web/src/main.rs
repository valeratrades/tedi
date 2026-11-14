//! The client bundle: `dx serve` / `dx bundle` build it; `tedi web` serves it and the API it calls.

use dioxus::prelude::*;

const TAILWIND: Asset = asset!("/assets/tailwind.css");

fn main() {
	dioxus::launch(|| {
		rsx! {
			document::Stylesheet { href: TAILWIND }
			document::Title { "tedi" }
			tedi_web::ui::App {}
		}
	});
}
