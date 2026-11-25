// What the page shows before the client runs: its frame, empty, and in place of the dock how much of the client is here.
(() => {
	const frame = document.createElement("div");
	frame.className = "fixed inset-0 z-10 flex bg-background text-ink";
	frame.dataset.testid = "frame";
	frame.innerHTML = `
		<div class="hidden w-(--size-rail) shrink-0 border-r border-border-subtle bg-card p-2 md:block"><div class="flex h-9 items-center pl-2 text-h3">tedi</div></div>
		<div class="flex min-w-0 flex-1 flex-col">
			<div class="h-(--size-topbar) shrink-0 border-b border-border-subtle bg-muted"></div>
			<div class="flex flex-1 flex-col items-center justify-center gap-8 text-primary-ink">
				<span data-slot="logo" aria-hidden="true" class="hidden h-11 [[data-mark]_&]:block" style="background-color:currentColor;mask:var(--brand-mark) center/contain no-repeat;aspect-ratio:var(--brand-aspect,1)"></span>
				<div role="progressbar" aria-label="Loading" aria-valuenow="0" class="flex w-48 max-w-[60%] flex-col items-center gap-6">
					<div class="h-px w-full overflow-hidden bg-border"><div class="h-full bg-current" style="width:0"></div></div>
					<span class="pl-[0.2em] text-mono-s tracking-[0.2em] text-ink-soft">0%</span>
				</div>
			</div>
		</div>`;
	const bar = frame.querySelector("[role]");
	document.documentElement.append(frame); // the body isn't parsed yet
	requestAnimationFrame(() => {
		// the first frame: the theme's sheet has loaded
		document.documentElement.toggleAttribute("data-mark", !!getComputedStyle(frame).getPropertyValue("--brand-mark"));
	});

	const failed = (why) => {
		const said = document.createElement("p");
		said.role = "alert";
		said.className = "max-w-sm text-center text-body-s text-accent-error";
		said.textContent = `The board didn't start: ${why}`;
		bar.replaceWith(said);
	};
	const starting = new AbortController();
	addEventListener("unhandledrejection", (e) => failed(e.reason), { signal: starting.signal });
	addEventListener("error", (e) => failed(e.message ?? `${e.target.src ?? e.target.href} didn't load`), { signal: starting.signal, capture: true });
	window.loading = {
		started() {
			starting.abort();
			frame.remove();
		},
	};

	// asked for along with the client, which takes them over; the board, the most of it, is the client's to ask for, in the order of what it shows
	const base = new URL("..", document.currentScript.src);
	const fetched = window.fetch.bind(window);
	const early = new Map(["changes", "setup", "library"].map((path) => [new URL(`api/${path}`, base).href, fetched(new URL(`api/${path}`, base))]));
	window.fetch = async (input, init) => {
		const url = new URL(input instanceof Request ? input.url : input, location).href;
		if (early.has(url)) {
			const response = early.get(url);
			early.delete(url);
			return response;
		}
		const response = await fetched(input, init);
		if (new URL(url).pathname.endsWith(".wasm") && response.ok) count(response.clone());
		return response;
	};
	/** `x-bytes`, which survives a proxy's compression; a dev server tells the length only. */
	const count = async ({ headers, url, body }) => {
		const of = Number(headers.get("x-bytes") ?? (headers.has("content-encoding") ? Number.NaN : headers.get("content-length")));
		if (!(of > 0)) throw new Error(`${url} doesn't say how long it is`);
		const reader = body.getReader();
		for (let loaded = 0, read; !(read = await reader.read()).done; ) {
			loaded += read.value.length;
			const percent = Math.min(99, Math.floor((100 * loaded) / of)); // 100 is the client running
			bar.ariaValueNow = percent;
			bar.lastElementChild.textContent = `${percent}%`;
			bar.querySelector("[style]").style.width = `${percent}%`;
		}
	};
})();
