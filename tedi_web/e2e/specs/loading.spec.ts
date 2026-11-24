import { expect, type Page, test } from "@playwright/test";

const bar = (page: Page) => page.getByRole("progressbar", { name: "Loading" });
const card = (page: Page) => page.locator('a[href*="issue=service-arb/"]').first();
const shown = async (page: Page) => {
  await expect(card(page)).toBeVisible();
  await expect(bar(page)).toHaveCount(0);
};
/** What the bar reads, in each of the ways it says it. */
const reads = async (page: Page, percent: number) => {
  await expect(bar(page)).toHaveText(`${percent}%`);
  await expect(bar(page)).toHaveAttribute("aria-valuenow", String(percent));
  const [track, fill] = await Promise.all(["track", "fill"].map(async (n) => (await bar(page).getByTestId(n).boundingBox())!.width));
  expect(Math.abs((100 * fill) / track - percent)).toBeLessThanOrEqual(1);
};

type Held = { release(what: "client" | "board", pieces: number): void };

const STORE_WEIGHS = 10_000; // the seeded store is nothing beside a debug client

/** The client (in 8 pieces) and the board (a line each, of so many times its bytes) reach the page only as `window.held` releases them. */
const hold = (page: Page) =>
  page.addInitScript((weighs) => {
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
      return new TextDecoder().decode(body).trim().split("\n").map((line) => {
        const part = JSON.parse(line);
        if (part.Read) part.Read.bytes *= weighs;
        return new TextEncoder().encode(JSON.stringify(part) + "\n");
      });
    };
    window.fetch = async (input, init) => {
      const url = input instanceof Request ? input.url : String(input);
      const what = /\.wasm$/.test(url) ? "client" : /\/api\/board$/.test(url) ? "board" : null;
      const response = await fetched(input, init);
      if (!what || !response.ok) return response;
      const all = pieces(what, new Uint8Array(await response.arrayBuffer()));
      let sent = 0;
      const body = new ReadableStream({
        async pull(to) {
          if (sent === all.length) return to.close();
          while (sent >= released[what]) await new Promise<void>((go) => waiting.push(go));
          to.enqueue(all[sent++]);
        },
      });
      const headers = new Headers(response.headers);
      if (what === "board") headers.set("x-bytes", String(weighs * Number(headers.get("x-bytes"))));
      return Object.defineProperty(new Response(body, { headers }), "url", { value: response.url });
    };
  }, STORE_WEIGHS);
const release = (page: Page, what: "client" | "board", pieces: number) => page.evaluate(([w, n]) => (window as never as { held: Held }).held.release(w as "client", n as number), [what, pieces] as const);

/** The sizes the page is told, and the bytes of the store each line of the board holds. */
const sizes = async (page: Page) => {
  const wasm = await page.request.get("/wasm/tedi_web_bg.wasm");
  const board = await page.request.get("/api/board");
  const parts = (await board.text()).trim().split("\n").map((l) => STORE_WEIGHS * ((JSON.parse(l).Read?.bytes ?? 0) as number));
  return { client: Number(wasm.headers()["x-bytes"]), store: STORE_WEIGHS * Number(board.headers()["x-bytes"]), parts };
};

test("the percentage is what is loaded of what there is to, on a first load, to the percent", async ({ page }) => {
  await page.request.get("/"); // a viewer
  const { client, store, parts } = await sizes(page);
  expect(client).toBeGreaterThan(0);
  expect(parts.reduce((a, b) => a + b)).toBe(store);
  expect((100 * store) / (client + store), "the board shows in the percentage").toBeGreaterThan(20);
  const percent = (loaded: number) => Math.min(99, Math.floor((100 * loaded) / (client + store)));

  await hold(page);
  await page.goto("/");
  await reads(page, 0);
  for (const pieces of [1, 4, 7]) {
    await release(page, "client", pieces);
    await reads(page, percent(Math.floor((pieces * client) / 8)));
  }
  await release(page, "client", 8);
  await reads(page, percent(client));
  for (const at of [0.25, 0.5, 0.75].map((f) => Math.floor(f * parts.length))) {
    await release(page, "board", at);
    await reads(page, percent(client + parts.slice(0, at).reduce((a, b) => a + b, 0)));
  }
  await release(page, "board", parts.length - 1);
  await reads(page, 99);
  await release(page, "board", parts.length);
  await shown(page);
});

test("it is in the middle of the screen, over the dock, its grips included", async ({ page }) => {
  await hold(page);
  await page.goto("/");
  const { x, y, width, height } = (await page.getByTestId("loading").boundingBox())!;
  const { width: w, height: h } = page.viewportSize()!;
  expect(Math.abs(x + width / 2 - w / 2)).toBeLessThan(1);
  expect(Math.abs(y + height / 2 - h / 2)).toBeLessThan(1);
  await release(page, "client", 8);
  const grips = page.locator("[class*=tl-resize]");
  await expect.poll(() => grips.count()).toBeGreaterThan(0);
  const over = (e: Element) => {
    const { x, y, width, height } = e.getBoundingClientRect();
    return e.contains(document.elementFromPoint(x + width / 2, y + height / 2));
  };
  expect(await grips.evaluateAll((all, over) => all.filter(eval(over)).length, over.toString())).toBe(0);
  await release(page, "board", Infinity);
  await shown(page);
});

test("the theme's mark is over the bar", async ({ page }) => {
  const svg = encodeURIComponent('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 2 1"><path d="M0 0h2v1H0z"/></svg>');
  await page.route(/\/assets\/.*\.css$/, async (route) => {
    const sheet = await route.fetch();
    await route.fulfill({ response: sheet, body: `${await sheet.text()} :root { --brand-mark: url("data:image/svg+xml,${svg}"); --brand-aspect: 2 / 1 }` });
  });
  await hold(page);
  await page.goto("/");
  const mark = page.getByTestId("loading").locator('[data-slot="logo"]');
  await expect(mark).toBeVisible();
  const [m, b] = [(await mark.boundingBox())!, (await bar(page).boundingBox())!];
  expect(m.y + m.height).toBeLessThan(b.y);
  expect(m.width).toBeCloseTo(2 * m.height);
});

test("a load that fails says why", async ({ page }) => {
  await page.route(/\/api\/board$/, (route) => route.fulfill({ status: 500, json: { ServerError: { message: "the store is gone", code: 500 } } }));
  await page.goto("/");
  await expect(page.getByText("The board didn't load")).toBeVisible();
  await expect(bar(page)).toHaveCount(0);
});

test("a board cut short is no board", async ({ page }) => {
  await page.route(/\/api\/board$/, async (route) => {
    const whole = await route.fetch();
    const body = (await whole.text()).split("\n").slice(0, 3).map((l) => l + "\n").join("");
    await route.fulfill({ response: whole, body });
  });
  await page.goto("/");
  await expect(page.getByText("The board didn't load")).toBeVisible();
});

test("a client that doesn't start says so", async ({ page }) => {
  await page.route(/\.wasm$/, (route) => route.abort());
  await page.goto("/");
  await expect(page.getByRole("alert")).toBeVisible();
});
