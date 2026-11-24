// The screen until the board shows: up before the client arrives, taken down by it (`ui/mod.rs`).
// Each part of the wait is drawn at the pace it took the last time.
(() => {
	const KEY = "tedi.loading";
	const PARTS = 3; // the client; setup and the library; the board
	const last = JSON.parse(localStorage.getItem(KEY)); // null: never loaded here
	const ended = [];
	const sum = (parts) => parts.reduce((a, b) => a + b, 0);

	const screen = document.createElement("div");
	screen.className = "fixed inset-0 z-[110] flex items-center justify-center bg-background text-primary-ink";
	screen.innerHTML = `
		<div data-testid="loading" class="flex flex-col items-center gap-8">
			<span data-slot="logo" aria-hidden="true" class="h-11" hidden style="background-color:currentColor;mask:var(--brand-mark) center/contain no-repeat;aspect-ratio:var(--brand-aspect,1)"></span>
			<div role="progressbar" aria-label="Loading" class="flex w-48 flex-col items-center gap-6">
				<div class="h-px w-full overflow-hidden bg-border"><div class="h-full origin-left bg-current" style="transform:scaleX(0)"></div></div>
				<span class="h-4 pl-[0.2em] text-mono-s tracking-[0.2em] text-ink-soft"></span>
			</div>
		</div>`;
	const bar = screen.querySelector("[role]");
	const fill = bar.querySelector("[style]");
	document.documentElement.append(screen); // the body isn't parsed yet

	const pace = (part) => {
		const from = sum(last.slice(0, part)) / sum(last);
		const share = last[part] / sum(last);
		const duration = 5 * last[part]; // past its time it creeps over the last tenth
		return fill.animate(
			[
				{ transform: getComputedStyle(fill).transform },
				{ transform: `scaleX(${from})`, offset: Math.min(0.1, 150 / duration) },
				{ transform: `scaleX(${from + 0.9 * share})`, offset: 0.2 },
				{ transform: `scaleX(${from + share})` },
			],
			{ duration, fill: "forwards" },
		);
	};
	const draw = () => {
		if (!bar.isConnected) return;
		const at = Math.min(99, Math.floor(100 * new DOMMatrix(getComputedStyle(fill).transform).a));
		bar.ariaValueNow = at;
		bar.lastElementChild.textContent = `${at}%`;
		requestAnimationFrame(draw);
	};
	requestAnimationFrame(() => {
		// the first frame: the theme's sheet has loaded
		screen.querySelector("[data-slot=logo]").hidden = !getComputedStyle(screen).getPropertyValue("--brand-mark");
		if (last) {
			if (ended.length === 0) pace(0).currentTime = performance.now(); // it began with the navigation
			draw();
		} else {
			fill.animate([{ transform: "translateX(-40%) scaleX(0.4)" }, { transform: "translateX(100%) scaleX(0.4)" }], { duration: 1600, iterations: Infinity, easing: "ease-in-out" });
		}
	});

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
		reached(parts) {
			starting.abort();
			if (!screen.isConnected || parts <= ended.length) return;
			while (ended.length < parts) ended.push(performance.now());
			if (parts < PARTS) {
				if (last) pace(parts);
				return;
			}
			localStorage.setItem(KEY, JSON.stringify(ended.map((at, i) => at - (ended[i - 1] ?? 0))));
			screen.style.pointerEvents = "none";
			screen.animate({ opacity: 0 }, { duration: 200 }).finished.then(() => screen.remove());
		},
		quit() {
			screen.remove();
		},
	};
})();
