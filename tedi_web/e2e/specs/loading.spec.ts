import { readFileSync } from "node:fs";

import { expect, type Locator, type Page, test } from "@playwright/test";

const bars = (within: Page | Locator) => within.getByRole("progressbar", { name: "Loading" });
const shelf = (page: Page, name: string) => page.locator("aside").getByRole("link", { name, exact: true });
const tile = (page: Page) => page.locator('.tl-render-overlay[data-tile="0"]').filter({ visible: true }).first();
const search = (page: Page) => page.getByRole("textbox", { name: "Search", exact: true });
const cards = (within: Page | Locator) => within.locator('a[href*="issue=service-arb/"]');
const reads = async (bar: Locator, percent: number) => {
  await expect(bar).toHaveText(`${percent}%`);
  await expect(bar).toHaveAttribute("aria-valuenow", String(percent));
};
const share = (loaded: number, of: number) => Math.min(99, Math.floor((100 * loaded) / of));
const sum = (all: number[]) => all.reduce((a, b) => a + b, 0);

type Held = { release(what: "client" | "board", pieces: number): void };

/** The client (in 8 pieces) and the board (a line each) reach the page only as `release` lets them. */
const hold = (page: Page) =>
  page.addInitScript(() => {
    const fetched = window.fetch;
    const released = { client: 0, board: 0 };
    const waiting: (() => void)[] = [];
    Object.assign(window, {
      held: {
        release(what: "client" | "board", pieces: number) {
          released[what] = pieces;
          waiting.splice(0).forEach((go) => go());
        },
      },
    });
    const pieces = (what: "client" | "board", body: Uint8Array) => {
      if (what === "client") return Array.from({ length: 8 }, (_, i) => body.slice(Math.floor((i * body.length) / 8), Math.floor(((i + 1) * body.length) / 8)));
      const all = new TextDecoder().decode(body).trim().split("\n").map((line) => JSON.parse(line));
      const heavy = (window as never as { heavy?: string }).heavy && all.findLast((l) => l.Read?.source.holds === (window as never as { heavy: string }).heavy).Read;
      if (heavy) {
        const size = all[0].Head.sizes.find(([s]: [{ repo: string; holds: string }]) => s.repo === heavy.source.repo && s.holds === heavy.source.holds);
        size[1] += 100_000;
        heavy.bytes += 100_000;
      }
      return all.map((line) => new TextEncoder().encode(JSON.stringify(line) + "\n"));
    };
    window.fetch = async (input, init) => {
      const url = input instanceof Request ? input.url : String(input);
      const what = /\.wasm$/.test(url) ? "client" : /\/api\/board$/.test(url) ? "board" : null;
      const response = await fetched(input, init);
      if (!what || !response.ok) return response;
      const all = pieces(what, new Uint8Array(await response.arrayBuffer()));
      if (what === "board") Object.assign(window, { lines: all.map((l) => JSON.parse(new TextDecoder().decode(l))).map((l) => ({ ranks: !!l.Ranks, bytes: l.Read?.bytes ?? 0, repo: l.Read?.source.repo, holds: l.Read?.source.holds })) });
      let sent = 0;
      const body = new ReadableStream({
        async pull(to) {
          if (sent === all.length) return to.close();
          while (sent >= released[what]) await new Promise<void>((go) => waiting.push(go));
          to.enqueue(all[sent++]);
        },
      });
      return Object.defineProperty(new Response(body, { headers: response.headers }), "url", { value: response.url });
    };
  });
const release = (page: Page, what: "client" | "board", pieces: number) => page.evaluate(([w, n]) => (window as never as { held: Held }).held.release(w as "client", n as number), [what, pieces] as const);

type Line = { ranks: boolean; bytes: number; repo?: string; holds?: "OpenIssues" | "ClosedIssues" | "PullRequests" };
/** The board's lines, as the page is sent them. */
const lines = async (page: Page) => (await page.waitForFunction(() => (window as never as { lines?: Line[] }).lines)).jsonValue() as Promise<Line[]>;
/** How much of the lines `needed` is in the first `n`. */
const part = (all: Line[], n: number, needed: (l: Line) => boolean) => share(sum(all.slice(0, n).filter(needed).map((l) => l.bytes)), sum(all.filter(needed).map((l) => l.bytes)));
/** Where the last line of those `needed` is. */
const last = (all: Line[], needed: (l: Line) => boolean) => all.findLastIndex(needed);
const signIn = (page: Page) => page.request.get("/");

test("before the client: the page's frame, and how much of the client is here", async ({ page, viewport }) => {
  await signIn(page);
  const client = Number((await page.request.get("/wasm/tedi_web_bg.wasm")).headers()["x-bytes"]);
  await hold(page);
  await page.goto("/");
  await reads(bars(page), 0);
  if (viewport!.width >= 768) await expect(page.getByTestId("frame").getByText("tedi")).toBeVisible();
  for (const pieces of [1, 4, 7]) {
    await release(page, "client", pieces);
    await reads(bars(page), share(Math.floor((pieces * client) / 8), client));
  }
  await release(page, "client", 8);
  await expect(page.getByTestId("frame")).toHaveCount(0);
});

test.describe("what waits for the board is the views on screen, each for what it shows", () => {
  test.skip(({ viewport }) => viewport!.width < 768, "the shelves are behind a sheet");
  const openIssues = (l: Line) => l.holds === "OpenIssues";
  const itsPrs = (l: Line) => l.holds === "PullRequests" && l.repo!.startsWith("service-arb/"); // the scope on screen

  test("the shelves, the search, the dock and a view's own toolbar are there with none of it, and work", async ({ page }) => {
    await hold(page);
    await page.goto("/");
    await release(page, "client", 8);
    await expect(shelf(page, "kanban")).toBeVisible();
    await expect(search(page)).toHaveValue(/is:pr/);
    await reads(bars(tile(page)), 0);
    const [t, b] = [(await tile(page).boundingBox())!, (await bars(tile(page)).boundingBox())!];
    expect(Math.abs(b.x + b.width / 2 - (t.x + t.width / 2))).toBeLessThan(2);
    await shelf(page, "Triage desk").click();
    await expect(search(page)).toHaveValue(/is:issue/);
    await reads(bars(tile(page)), 0);
    await page.getByRole("button", { name: "filter by repo" }).click();
    await expect(page.getByText("Counted once the view is here")).toBeVisible();
  });

  test("a view is drawn behind its loader, which covers it whole", async ({ page }) => {
    await hold(page);
    await page.goto("/");
    await release(page, "client", 8);
    await reads(bars(tile(page)), 0);
    const heads = tile(page).locator(".cols .tl-tab");
    await expect(heads).not.toHaveCount(0);
    const covered = (e: Element) => {
      const { x, y, width, height } = e.getBoundingClientRect();
      return !e.contains(document.elementFromPoint(x + width / 2, y + height / 2));
    };
    const within = tile(page).locator(".cols :is(.tl-tab, [class*=tl-resize])");
    expect(await within.evaluateAll((all, covered) => all.every(eval(covered)), covered.toString())).toBe(true);
  });

  test("a view of open issues waits for those, whatever else is still to come", async ({ page }) => {
    await hold(page);
    await page.goto("/");
    await release(page, "client", 8);
    const all = await lines(page);
    await shelf(page, "Triage desk").click();
    await expect(search(page)).toHaveValue(/is:issue/);
    const issues = last(all, openIssues) + 1;
    expect(all.findIndex((l) => l.ranks), "ranked as soon as they are read").toBe(issues);
    for (const n of [0.4, 0.8].map((f) => Math.ceil(f * issues))) {
      await release(page, "board", n);
      await reads(bars(tile(page)), part(all, n, openIssues));
    }
    await release(page, "board", issues);
    await reads(bars(tile(page)), 99);
    await release(page, "board", issues + 1);
    await expect(cards(tile(page)).first()).toBeVisible();
    await expect(bars(tile(page))).toHaveCount(0);
    expect(issues + 1, "the rest is still held").toBeLessThan(all.length);
  });

  test("a view of pull requests waits for its scope's; the backend is told its percentage was off", async ({ page }) => {
    await page.addInitScript(() => Object.assign(window, { heavy: "PullRequests" })); // its last line, weighing most of it
    await hold(page);
    await page.goto("/");
    await release(page, "client", 8);
    const all = await lines(page);
    const end = last(all, itsPrs) + 1;
    for (let n = all.findIndex(itsPrs) + 1; n < end; n++) {
      await release(page, "board", n);
      await reads(bars(tile(page)), part(all, n, itsPrs));
    }
    await page.waitForTimeout(250); // a shorter wait is none, whatever it read
    const off = page.waitForRequest(/\/api\/off$/);
    await release(page, "board", end);
    await expect(cards(tile(page)).first()).toBeVisible();
    await expect(bars(tile(page))).toHaveCount(0);

    // its last line was more than the 5% a percentage may be off by: the backend is told
    const percent = part(all, end - 1, itsPrs);
    expect(99 - percent).toBeGreaterThan(5);
    expect((await off).postDataJSON()).toMatchObject({ what: "view Release checklist", percent });
    await expect.poll(() => readFileSync("/tmp/tedi-e2e-specs/server.log", "utf8")).toContain("a view's progress was off");
  });

});

test("the theme's mark is over the bar", async ({ page }) => {
  const svg = encodeURIComponent('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 2 1"><path d="M0 0h2v1H0z"/></svg>');
  await page.route(/\/assets\/.*\.css$/, async (route) => {
    const sheet = await route.fetch();
    await route.fulfill({ response: sheet, body: `${await sheet.text()} :root { --brand-mark: url("data:image/svg+xml,${svg}"); --brand-aspect: 2 / 1 }` });
  });
  await hold(page);
  await page.goto("/");
  for (const within of [page.getByTestId("frame"), tile(page)]) {
    const mark = within.locator('[data-slot="logo"]').first();
    await expect(mark).toBeVisible();
    const [m, b] = [(await mark.boundingBox())!, (await bars(within).first().boundingBox())!];
    expect(m.y + m.height).toBeLessThan(b.y);
    expect(m.width).toBeCloseTo(2 * m.height);
    await release(page, "client", 8);
  }
});

test("a load that fails says why, where it would have been", async ({ page }) => {
  await page.route(/\/api\/board$/, (route) => route.fulfill({ status: 500, json: { ServerError: { message: "the store is gone", code: 500 } } }));
  await page.goto("/");
  await expect(tile(page).getByText("The board didn't load")).toBeVisible();
  await expect(bars(page)).toHaveCount(0);
});

test("a board cut short is no board", async ({ page }) => {
  await page.route(/\/api\/board$/, async (route) => {
    const whole = await route.fetch();
    const body = (await whole.text()).split("\n").slice(0, 3).map((l) => l + "\n").join("");
    await route.fulfill({ response: whole, body });
  });
  await page.goto("/");
  await expect(page.getByText("The board didn't load").first()).toBeVisible();
  await page.unrouteAll({ behavior: "ignoreErrors" }); // a later read of it may be under way
});

test("a client that doesn't start says so", async ({ page }) => {
  await page.route(/\.wasm$/, (route) => route.abort());
  await page.goto("/");
  await expect(page.getByRole("alert")).toBeVisible();
});
