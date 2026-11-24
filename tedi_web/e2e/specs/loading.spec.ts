import { expect, type Locator, type Page, test } from "@playwright/test";

const bars = (within: Page | Locator) => within.getByRole("progressbar", { name: "Loading" });
const shelf = (page: Page, name: string) => page.locator("aside").getByRole("link", { name, exact: true });
const tile = (page: Page) => page.locator('.tl-render-overlay[data-tile="0"]').filter({ visible: true }).first();
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
      return new TextDecoder().decode(body).trim().split("\n").map((line) => new TextEncoder().encode(line + "\n"));
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
      return Object.defineProperty(new Response(body, { headers: response.headers }), "url", { value: response.url });
    };
  });
const release = (page: Page, what: "client" | "board", pieces: number) => page.evaluate(([w, n]) => (window as never as { held: Held }).held.release(w as "client", n as number), [what, pieces] as const);

/** The board's lines: the issues' bytes and the pull requests', and the line the issues are whole with. */
const lines = async (page: Page) => {
  const all = (await (await page.request.get("/api/board")).text()).trim().split("\n").map((l) => JSON.parse(l));
  const bytes = all.map((l) => (l.Read?.bytes ?? 0) as number);
  const whole = all.findIndex((l) => l.Rest);
  const { issues, prs } = all[0].Sizes as { issues: number; prs: number };
  expect(sum(bytes.slice(0, whole))).toBe(issues);
  expect(sum(bytes.slice(whole))).toBe(prs);
  return { bytes, whole, issues, prs, all: all.length };
};
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

  test("the shelves, the dock and a view's own toolbar are there with none of it, and work", async ({ page }) => {
    await hold(page);
    await page.goto("/");
    await release(page, "client", 8);
    await expect(shelf(page, "Triage")).toBeVisible();
    await expect(tile(page).getByRole("textbox")).toHaveValue(/is:pr/);
    await reads(bars(tile(page)), 0);
    const [t, b] = [(await tile(page).boundingBox())!, (await bars(tile(page)).boundingBox())!];
    expect(b.x).toBeGreaterThan(t.x);
    expect(b.x + b.width).toBeLessThan(t.x + t.width);
    expect(Math.abs(b.x + b.width / 2 - (t.x + t.width / 2))).toBeLessThan(2);
    await shelf(page, "Triage").click();
    await expect(tile(page).getByRole("textbox")).toHaveValue(/is:issue/);
    await reads(bars(tile(page)), 0);
    await tile(page).getByRole("button", { name: "filter by repo" }).click();
    await expect(page.getByText("Counted once the board is here")).toBeVisible();
  });

  test("a view of issues shows once they are here, one of pull requests reads both to the percent", async ({ page }) => {
    await signIn(page);
    const { bytes, whole, issues, prs, all } = await lines(page);
    await hold(page);
    await page.goto("/");
    await release(page, "client", 8);
    await shelf(page, "Triage").click();
    await expect(tile(page).getByRole("textbox")).toHaveValue(/is:issue/);
    const triage = tile(page);
    for (const at of [0.3, 0.7].map((f) => Math.ceil(f * whole))) {
      await release(page, "board", at);
      await reads(bars(triage), share(sum(bytes.slice(0, at)), issues));
    }
    await release(page, "board", whole);
    await reads(bars(triage), 99);
    await release(page, "board", whole + 1);
    await expect(cards(triage).first()).toBeVisible();
    await expect(bars(triage)).toHaveCount(0);

    await shelf(page, "Release checklist").click();
    await expect(tile(page).getByRole("textbox")).toHaveValue(/is:pr/);
    const checklist = tile(page);
    await reads(bars(checklist), share(issues, issues + prs));
    await release(page, "board", all - 1);
    await reads(bars(checklist), 99);
    await release(page, "board", all);
    await expect(cards(checklist).first()).toBeVisible();
    await expect(bars(page)).toHaveCount(0);
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
});

test("a client that doesn't start says so", async ({ page }) => {
  await page.route(/\.wasm$/, (route) => route.abort());
  await page.goto("/");
  await expect(page.getByRole("alert")).toBeVisible();
});
