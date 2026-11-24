// The screen until the board shows: up before the client arrives, taken down by it (`ui/mod.rs`).
// What it reads is the bytes loaded of those there are to load: the client's, and the store's the board is read from.
(() => {
	const parts = { client: { bytes: null, loaded: 0 }, board: { bytes: null, loaded: 0 } };

	const screen = document.createElement("div");
	screen.className = "fixed inset-0 z-[110] flex items-center justify-center bg-background text-primary-ink";
	screen.innerHTML = `
		<div data-testid="loading" class="flex flex-col items-center gap-8">
			<span data-slot="logo" aria-hidden="true" class="h-11" hidden style="background-color:currentColor;mask:var(--brand-mark) center/contain no-repeat;aspect-ratio:var(--brand-aspect,1)"></span>
			<div role="progressbar" aria-label="Loading" aria-valuenow="0" class="flex w-48 flex-col items-center gap-6">
				<div data-testid="track" class="h-px w-full overflow-hidden bg-border"><div data-testid="fill" class="h-full bg-current" style="width:0"></div></div>
				<span class="pl-[0.2em] text-mono-s tracking-[0.2em] text-ink-soft">0%</span>
			</div>
		</div>`;
	const bar = screen.querySelector("[role]");
	document.documentElement.append(screen); // the body isn't parsed yet
	requestAnimationFrame(() => {
		// the first frame: the theme's sheet has loaded
		screen.querySelector("[data-slot=logo]").hidden = !getComputedStyle(screen).getPropertyValue("--brand-mark");
	});

	const read = (percent) => {
		bar.ariaValueNow = percent;
		bar.lastElementChild.textContent = `${percent}%`;
	};
	const draw = () => {
		const all = Object.values(parts);
		if (all.some((p) => p.bytes === null)) return; // what there is to load isn't known yet
		const share = all.reduce((n, p) => n + p.loaded, 0) / all.reduce((n, p) => n + p.bytes, 0);
		read(Math.min(99, Math.floor(100 * share))); // 100 is the board on screen
		bar.querySelector("[style]").style.width = `${Math.min(99, 100 * share)}%`;
	};
	/** `x-bytes`, which survives a proxy's compression; a dev server tells the length only. */
	const sized = (part, { headers, url }) => {
		const bytes = headers.get("x-bytes") ?? (headers.has("content-encoding") ? null : headers.get("content-length"));
		if (bytes === null) throw new Error(`${url} doesn't say how long it is`);
		part.bytes = Number(bytes);
		draw();
	};

	// asked for along with the client, which takes them over; the board once its changes are followed
	const base = new URL("..", document.currentScript.src);
	const fetched = window.fetch.bind(window);
	const get = (path) => fetched(new URL(`api/${path}`, base));
	const changes = get("changes");
	const board = changes.then(() => get("board"));
	board.then((response) => response.ok && sized(parts.board, response));
	const early = new Map(Object.entries({ changes, board, setup: get("setup"), library: get("library") }).map(([path, response]) => [new URL(`api/${path}`, base).href, response]));

	window.fetch = async (input, init) => {
		const url = new URL(input instanceof Request ? input.url : input, location).href;
		const asked = early.get(url);
		if (asked) {
			early.delete(url);
			return asked;
		}
		const response = await fetched(input, init);
		if (new URL(url).pathname.endsWith(".wasm") && response.ok) {
			sized(parts.client, response);
			const body = response.clone().body.getReader();
			const count = ({ done, value }) => {
				if (done) return;
				parts.client.loaded += value.length;
				draw();
				return body.read().then(count);
			};
			body.read().then(count);
		}
		return response;
	};

	const starting = new AbortController();
	const failed = (why) => {
		const said = document.createElement("p");
		said.role = "alert";
		said.className = "max-w-sm text-center text-body-s text-accent-error";
		said.textContent = `The board didn't start: ${why}`;
		screen.replaceChildren(said);
	};
	addEventListener("unhandledrejection", (e) => failed(e.reason), { signal: starting.signal });
	addEventListener("error", (e) => failed(e.message ?? `${e.target.src ?? e.target.href} didn't load`), { signal: starting.signal, capture: true });

	window.loading = {
		started() {
			starting.abort();
		},
		loaded(bytes) {
			parts.board.loaded = bytes;
			draw();
		},
		shown() {
			read(100);
			screen.style.pointerEvents = "none";
			screen.animate({ opacity: 0 }, { duration: 200 }).finished.then(() => screen.remove());
		},
		quit() {
			screen.remove();
		},
	};
})();
